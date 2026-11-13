use crate::interpreter::interpreter;
use dap::{
    events::{Event, OutputEventBody, StoppedEventBody},
    requests::{Command, Request},
    responses::{
        ContinueResponse, ResponseBody, ScopesResponse, SetBreakpointsResponse, StackTraceResponse,
        ThreadsResponse, VariablesResponse,
    },
    server::Server,
    types::{
        Breakpoint, Capabilities, OutputEventCategory, Scope, ScopePresentationhint, Source,
        StackFrame, StoppedEventReason, Variable,
    },
};
use std::{
    io::{self, BufReader, BufWriter},
    ops::ControlFlow,
    sync::{Arc, Mutex, mpsc},
    thread,
};
use wipple_core::codegen::mir;
use wipple_interpreter::DebugTree;

enum State {
    Running,
    Paused,
    Stopped,
}

pub fn debug(program: &mir::Program) -> anyhow::Result<()> {
    let mut server = Server::new(BufReader::new(io::stdin()), BufWriter::new(io::stdout()));
    let output = server.output.clone();

    let mut interpreter = interpreter(program, {
        let output = output.clone();
        move |s| {
            output
                .lock()
                .unwrap()
                .send_event(Event::Output(OutputEventBody {
                    category: Some(OutputEventCategory::Console),
                    output: format!("{s}\n"),
                    ..Default::default()
                }))?;

            Ok(())
        }
    });

    let breakpoints: Arc<Mutex<_>> = Default::default();
    let (state_tx, state_rx) = mpsc::channel();
    let (stack_trace_tx, stack_trace_rx) = mpsc::channel();
    let mut ctx = Context {
        breakpoints: breakpoints.clone(),
        variables: Default::default(),
        state_tx,
        stack_trace_rx,
        output: output.clone(),
    };

    thread::scope(|scope| {
        let server_thread = scope.spawn(move || -> anyhow::Result<()> {
            while let Some(request) = server.poll_request()? {
                ctx.handle(request)?;
            }

            Ok(())
        });

        let mut state = State::Paused;
        'outer: loop {
            if let Ok(next) = state_rx.try_recv() {
                state = next;
            }

            match state {
                State::Running => {
                    let (ref path, ref breakpoints, ref mut paused) = *breakpoints.lock().unwrap();

                    if !*paused && let Some(path) = path {
                        for (index, breakpoint) in breakpoints.iter().enumerate() {
                            if interpreter.matches_breakpoint(
                                path,
                                breakpoint.line as usize,
                                breakpoint.column.map(|column| column as usize),
                            ) {
                                state = State::Paused;

                                output.lock().unwrap().send_event(Event::Stopped(
                                    StoppedEventBody {
                                        reason: StoppedEventReason::Breakpoint,
                                        description: None,
                                        thread_id: Some(1),
                                        preserve_focus_hint: None,
                                        text: None,
                                        all_threads_stopped: None,
                                        hit_breakpoint_ids: Some(vec![index as i64]),
                                    },
                                ))?;

                                *paused = true;

                                stack_trace_tx.send(interpreter.stack_trace()).unwrap();

                                continue 'outer;
                            }
                        }
                    }

                    *paused = false;

                    if let ControlFlow::Break(()) = interpreter.step()? {
                        state = State::Stopped;
                        output.lock().unwrap().send_event(Event::Terminated(None))?;
                    }
                }
                State::Paused => state = state_rx.recv()?,
                State::Stopped => break 'outer,
            }
        }

        server_thread.join().unwrap()?;

        Ok(())
    })
}

struct Context<'a> {
    breakpoints: Arc<Mutex<(Option<String>, Vec<dap::types::SourceBreakpoint>, bool)>>,
    variables: (Vec<Vec<usize>>, Vec<(String, String, Vec<usize>)>),
    state_tx: mpsc::Sender<State>,
    stack_trace_rx: mpsc::Receiver<wipple_interpreter::StackTrace<'a, !>>,
    output: Arc<Mutex<dap::server::ServerOutput<io::Stdout>>>,
}

impl<'a> Context<'a> {
    fn handle(&mut self, request: Request) -> anyhow::Result<()> {
        match &request.command {
            Command::Initialize(_) => {
                self.respond(request.success(ResponseBody::Initialize(Capabilities {
                    supports_configuration_done_request: Some(true),
                    ..Default::default()
                })))?;
            }
            Command::Launch(_) => {
                self.respond(request.success(ResponseBody::Launch))?;
                self.event(Event::Initialized)?;
            }
            Command::SetBreakpoints(command) => {
                let path = command.source.path.clone();
                let new_breakpoints = command.breakpoints.clone().unwrap_or_default();

                *self.breakpoints.lock().unwrap() = (path.clone(), new_breakpoints.clone(), false);

                self.respond(
                    request.success(ResponseBody::SetBreakpoints(SetBreakpointsResponse {
                        breakpoints: new_breakpoints
                            .into_iter()
                            .enumerate()
                            .map(|(index, breakpoint)| Breakpoint {
                                id: Some(index as i64),
                                source: path.as_ref().map(|path| Source {
                                    path: Some(path.clone()),
                                    ..Default::default()
                                }),
                                line: Some(breakpoint.line),
                                column: breakpoint.column,
                                verified: true,
                                ..Default::default()
                            })
                            .collect(),
                    })),
                )?;
            }
            Command::ConfigurationDone => {
                self.respond(request.success(ResponseBody::ConfigurationDone))?;

                self.state_tx.send(State::Running)?;
            }
            Command::Pause(_) => {
                self.respond(request.success(ResponseBody::Pause))?;

                self.event(Event::Stopped(StoppedEventBody {
                    reason: StoppedEventReason::Pause,
                    description: None,
                    thread_id: Some(1),
                    preserve_focus_hint: None,
                    text: None,
                    all_threads_stopped: None,
                    hit_breakpoint_ids: None,
                }))?;

                self.state_tx.send(State::Paused)?;
            }
            Command::Continue(_) => {
                self.respond(request.success(ResponseBody::Continue(ContinueResponse {
                    ..Default::default()
                })))?;

                self.state_tx.send(State::Running)?;
            }
            Command::Threads => {
                self.respond(request.success(ResponseBody::Threads(ThreadsResponse {
                    threads: vec![dap::types::Thread {
                        id: 1,
                        name: String::from("main"),
                    }],
                })))?;
            }
            Command::StackTrace(_) => {
                let stack_trace = self.stack_trace_rx.recv().unwrap();

                self.update_variables(&stack_trace);

                self.respond(
                    request.success(ResponseBody::StackTrace(StackTraceResponse {
                        stack_frames: stack_trace
                            .frames
                            .iter()
                            .enumerate()
                            .filter_map(|(index, frame)| {
                                Some((
                                    index,
                                    frame.function.name.as_ref(),
                                    frame.statement?.span.as_ref()?,
                                ))
                            })
                            .map(|(index, name, span)| StackFrame {
                                id: index as i64,
                                source: Some(Source {
                                    path: Some(span.path.to_string()),
                                    ..Default::default()
                                }),
                                name: name.map_or_default(|name| name.to_string()),
                                line: span.start.line as i64,
                                column: span.start.column as i64,
                                ..Default::default()
                            })
                            .collect(),
                        ..Default::default()
                    })),
                )?;
            }
            Command::Scopes(command) => {
                let frame_id = command.frame_id;

                let (frames, variables) = &self.variables;
                match frames.get(frame_id as usize) {
                    Some(frame) => {
                        let variables = frame
                            .iter()
                            .map(|&index| &variables[index])
                            .collect::<Vec<_>>();

                        self.respond(request.success(ResponseBody::Scopes(ScopesResponse {
                            scopes: vec![Scope {
                                name: String::from("Local"),
                                presentation_hint: Some(ScopePresentationhint::Locals),
                                named_variables: Some(variables.len() as i64),
                                variables_reference: frame_id + 1, // can't be 0
                                ..Default::default()
                            }],
                        })))?;
                    }
                    None => {
                        self.respond(request.error("invalid stack frame"))?;
                    }
                }
            }
            Command::Variables(command) => {
                let reference = (command.variables_reference - 1) as usize; // can't be 0

                let (frames, variables) = &self.variables;

                let indices = if reference < frames.len() {
                    &frames[reference]
                } else if let Some((_, _, children)) = variables.get(reference - frames.len()) {
                    children
                } else {
                    self.respond(request.error("invalid variables reference"))?;

                    return Ok(());
                };

                let variables = indices
                    .iter()
                    .map(|&index| {
                        let (name, value, children) = &variables[index];

                        Variable {
                            name: name.clone(),
                            value: value.clone(),
                            variables_reference: if children.is_empty() {
                                0
                            } else {
                                (index + frames.len() + 1) as i64
                            },
                            ..Default::default()
                        }
                    })
                    .collect();

                self.respond(
                    request.success(ResponseBody::Variables(VariablesResponse { variables })),
                )?;
            }
            Command::Disconnect(_) => {
                self.respond(request.success(ResponseBody::Disconnect))?;

                self.state_tx.send(State::Stopped)?;
            }
            _ => {
                self.respond(request.error("unsupported request"))?;
            }
        }

        Ok(())
    }

    fn respond(&self, response: dap::responses::Response) -> anyhow::Result<()> {
        self.output.lock().unwrap().respond(response)?;
        Ok(())
    }

    fn event(&self, event: dap::events::Event) -> anyhow::Result<()> {
        self.output.lock().unwrap().send_event(event)?;
        Ok(())
    }

    fn update_variables(&mut self, stack_trace: &wipple_interpreter::StackTrace<'a, !>) {
        fn enqueue(
            name: String,
            tree: &DebugTree<!>,
            variables: &mut Vec<(String, String, Vec<usize>)>,
        ) {
            match tree {
                DebugTree::Value {
                    representation,
                    children,
                } => {
                    let index = variables.len();
                    variables.push((name, representation.clone(), Vec::new()));

                    let mut next_unnamed_child = 0;
                    let indices = children
                        .iter()
                        .map(|(key, child)| {
                            let index = variables.len();
                            enqueue(
                                key.clone().unwrap_or_else(|| {
                                    let index = next_unnamed_child;
                                    next_unnamed_child += 1;
                                    index.to_string()
                                }),
                                child,
                                variables,
                            );

                            index
                        })
                        .collect::<Vec<_>>();

                    variables[index].2 = indices;
                }
                DebugTree::External(never) => match *never {},
            }
        }

        let mut variables = Vec::new();
        let frames = stack_trace
            .frames
            .iter()
            .map(|frame| {
                frame
                    .variables
                    .iter()
                    .filter_map(|(debug_info, variable)| {
                        let name = debug_info.name.as_ref()?.to_string();

                        let index = variables.len();
                        enqueue(name, variable, &mut variables);

                        Some(index)
                    })
                    .collect()
            })
            .collect::<Vec<_>>();

        self.variables = (frames, variables);
    }
}
