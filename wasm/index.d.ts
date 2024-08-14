declare const init: any;
export default init;

export const wasm_thread_entry_point: any;

export const initialize: (options: any) => Promise<any>;
export const compile: (options: any) => Promise<any>;
export const format: (options: any) => Promise<any>;
export const highlights: (options: any) => Promise<any>;
export const help: (options: any) => Promise<any>;
export const getIntelligentFix: (options: any) => Promise<any>;
export const run: (options: any) => Promise<any>;
export const stop: (options: any) => Promise<any>;
export const cleanup: (options: any) => Promise<any>;
