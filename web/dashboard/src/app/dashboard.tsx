import { useCallback, useState } from "react";
import { CircularProgress, PlaygroundPageItem, PlaygroundPageItemEditor } from "wipple-playground";
import * as wipple from "wipple-wasm";
import { useSocketClient } from "../helpers";

export const Dashboard = () => {
    const [students, setStudents] = useState<{ id: string; name: string }[]>([]);

    const [selectedStudent, setSelectedStudent] = useState<{
        id: string;
        name: string;
        item: PlaygroundPageItem | undefined;
    }>();

    const [studentsAskingForHelp, setStudentsAskingForHelp] = useState<string[]>([]);

    const socketClient = useSocketClient({
        onUpdateStudents: (students) => {
            setStudents(students);

            setSelectedStudent((selectedStudent) => {
                if (
                    selectedStudent &&
                    !students.find((student) => student.id === selectedStudent.id)
                ) {
                    return undefined;
                }

                return selectedStudent;
            });
        },
        onUpdateStudent: (studentId, data) => {
            setSelectedStudent((selectedStudent) => {
                if (!selectedStudent || selectedStudent.id !== studentId) {
                    return selectedStudent;
                }

                return { ...selectedStudent, item: data };
            });
        },
        onAskForHelp: (studentId) => {
            setStudentsAskingForHelp((studentsAskingForHelp) => {
                if (studentsAskingForHelp.includes(studentId)) {
                    return studentsAskingForHelp;
                }

                return [...studentsAskingForHelp, studentId];
            });
        },
    });

    const selectStudent = useCallback(
        (studentId: string, studentName: string) => {
            if (selectedStudent) {
                socketClient?.unsubscribeFromStudent(selectedStudent.id);
            }

            setSelectedStudent({ id: studentId, name: studentName, item: undefined });

            socketClient?.subscribeToStudent(studentId);
        },
        [socketClient, selectedStudent],
    );

    return socketClient ? (
        <div className="flex flex-row w-screen h-screen">
            <div className="flex flex-col items-start gap-2 w-80 h-full bg-gray-50 dark:bg-gray-900 p-4">
                <h1 className="text-xl font-semibold">Active Students</h1>

                {students.map((student) => (
                    <button
                        key={student.id}
                        onClick={() => {
                            setStudentsAskingForHelp((studentsAskingForHelp) =>
                                studentsAskingForHelp.filter((id) => id !== student.id),
                            );

                            selectStudent(student.id, student.name);
                        }}
                        className={`w-full p-2 rounded-md text-left px-3 transition-colors ${
                            studentsAskingForHelp.includes(student.id)
                                ? "bg-red-500 text-white"
                                : selectedStudent?.id === student.id
                                ? "bg-blue-500 text-white"
                                : "text-gray-500 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-800"
                        }`}
                    >
                        {student.name}
                        {studentsAskingForHelp.includes(student.id) ? " — needs help" : null}
                    </button>
                ))}
            </div>

            {selectedStudent ? (
                <div className="w-full max-w-screen-lg mx-auto p-4">
                    {selectedStudent.item ? (
                        <DashboardItem
                            item={selectedStudent.item}
                            studentName={selectedStudent.name}
                        />
                    ) : null}
                </div>
            ) : (
                <div className="flex items-center justify-center w-full h-full">
                    <p className="text-xl font-medium text-gray-400 dark:text-gray-600">
                        No student selected
                    </p>
                </div>
            )}
        </div>
    ) : (
        <div className="flex items-center justify-center w-screen h-screen">
            <CircularProgress />
        </div>
    );
};

const DashboardItem = (props: { item: PlaygroundPageItem; studentName: string }) => (
    <PlaygroundPageItemEditor
        wipple={wipple}
        id={{ page: "dashboard", index: 0 }}
        item={props.item}
        onChange={() => {}}
        locked
        readOnly
        menu={<Menu studentName={props.studentName} />}
    />
);

const Menu = (props: { studentName: string }) => {
    return (
        <div className="flex flex-row p-2">
            <p className="text-xs text-gray-500 dark:text-gray-400">
                Viewing <strong>{props.studentName}</strong>
            </p>
        </div>
    );
};
