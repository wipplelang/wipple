import * as Tonal from "tonal";

let Tone: typeof import("tone");

export interface Instrument {
    init: () => Promise<void>;
    play: (options: { note: string; time?: number; duration?: number }) => void;
    stopAll: () => void;
}

export const getAudioContext = () => Tone.getContext();

export const defaultNoteDuration = 1;

const drumMachineBaseUrl = "https://smpldsnds.github.io/drum-machines/Casio-RZ1/";

const drumMachineSampleNames: Record<string, string> = {
    clap: "clap.m4a",
    clave: "clave.m4a",
    cowbell: "cowbell.m4a",
    crash: "crash.m4a",
    hihat: "hihat-closed.m4a",
    kick: "kick.m4a",
    ride: "ride.m4a",
    snare: "snare.m4a",
    tom: "tom-1.m4a",
};

let drumMachine: Instrument | undefined;
export const getDrumMachine = async (): Promise<Instrument> => {
    if (drumMachine) {
        return drumMachine;
    }

    await initializeAudio();

    const players = new Tone.Players(drumMachineSampleNames, {
        baseUrl: drumMachineBaseUrl,
        volume: -15,
    });

    const instrument: Instrument = {
        init: async () => {
            await Tone.loaded();
            players.toDestination();
        },
        play: ({ note, time }) => {
            if (!note) return;

            players.player(note).start(time, 0, time ?? Tone.getContext().currentTime);
        },
        stopAll: () => players.stopAll(),
    };

    drumMachine = instrument;

    return instrument;
};

const soundfontBaseUrl = "https://gleitz.github.io/midi-js-soundfonts/FluidR3_GM/";

export const soundfontInstrumentNames: Record<string, string> = {
    piano: "acoustic_grand_piano-mp3/",
    "electric-guitar": "electric_guitar_clean-mp3/",
    flute: "flute-mp3/",
    harmonica: "harmonica-mp3/",
    marimba: "marimba-mp3/",
    "orchestra-hit": "orchestra_hit-mp3/",
    strings: "string_ensemble_1-mp3/",
    trumpet: "trumpet-mp3/",
    tuba: "tuba-mp3/",
    violin: "violin-mp3/",
};

// Load one octave; other notes will be pitch-shifted from these
const noteSamples = Tonal.Range.chromatic(["C4", "B4"]);

const soundfontInstruments = Object.fromEntries(
    Object.keys(soundfontInstrumentNames).map((name) => [
        name,
        undefined as Instrument | undefined,
    ]),
);

export const getDefaultSoundfontInstrument = () => getSoundfontInstrument("piano");

export const getSoundfontInstrument = async (name: string): Promise<Instrument> => {
    if (soundfontInstruments[name] != null) {
        return soundfontInstruments[name];
    }

    await initializeAudio();

    const sampler = new Tone.Sampler({
        urls: Object.fromEntries(noteSamples.map((note) => [note, `${note}.mp3`])),
        baseUrl: soundfontBaseUrl + soundfontInstrumentNames[name],
    });

    const instrument: Instrument = {
        init: async () => {
            await Tone.loaded();
            sampler.toDestination().sync();
        },
        play: ({ note, time, duration }) => {
            if (!note) return;

            sampler.triggerAttackRelease(
                Tonal.Note.fromMidi(Tonal.Midi.toMidi(note)!), // normalize
                (duration ?? defaultNoteDuration) * 1.5,
                time ?? Tone.getContext().currentTime,
            );
        },
        stopAll: () => sampler.releaseAll(),
    };

    soundfontInstruments[name] = instrument;

    return instrument;
};

export const initializeAudio = async () => {
    Tone = await import("tone");
    await Tone.start();
    Tone.getTransport().start();
};

export const stopAllInstruments = () => {
    Tone.getTransport().cancel();

    for (const instrument of Object.values(soundfontInstruments)) {
        instrument?.stopAll();
    }

    drumMachine?.stopAll();
};
