// https://tailwindcss.com/docs/colors
const colors = {
    white: {
        varName: "--color-white",
        background: "bg-white",
        foreground: "text-white",
    },
    "gray 1": {
        varName: "--color-gray-200",
        background: "bg-gray-200",
        foreground: "text-gray-200",
    },
    "gray 2": {
        varName: "--color-gray-300",
        background: "bg-gray-300",
        foreground: "text-gray-300",
    },
    "gray 3": {
        varName: "--color-gray-500",
        background: "bg-gray-500",
        foreground: "text-gray-500",
    },
    "gray 4": {
        varName: "--color-gray-800",
        background: "bg-gray-800",
        foreground: "text-gray-800",
    },
    black: {
        varName: "--color-black",
        background: "bg-black",
        foreground: "text-black",
    },
    "light red": {
        varName: "--color-red-300",
        background: "bg-red-300",
        foreground: "text-red-300",
    },
    red: {
        varName: "--color-red-500",
        background: "bg-red-500",
        foreground: "text-red-500",
    },
    "dark red": {
        varName: "--color-red-800",
        background: "bg-red-800",
        foreground: "text-red-800",
    },
    "light orange": {
        varName: "--color-orange-300",
        background: "bg-orange-300",
        foreground: "text-orange-300",
    },
    orange: {
        varName: "--color-orange-500",
        background: "bg-orange-500",
        foreground: "text-orange-500",
    },
    "dark orange": {
        varName: "--color-orange-800",
        background: "bg-orange-800",
        foreground: "text-orange-800",
    },
    "light amber": {
        varName: "--color-amber-300",
        background: "bg-amber-300",
        foreground: "text-amber-300",
    },
    amber: {
        varName: "--color-amber-500",
        background: "bg-amber-500",
        foreground: "text-amber-500",
    },
    "dark amber": {
        varName: "--color-amber-800",
        background: "bg-amber-800",
        foreground: "text-amber-800",
    },
    "light yellow": {
        varName: "--color-yellow-300",
        background: "bg-yellow-300",
        foreground: "text-yellow-300",
    },
    yellow: {
        varName: "--color-yellow-500",
        background: "bg-yellow-500",
        foreground: "text-yellow-500",
    },
    "dark yellow": {
        varName: "--color-yellow-800",
        background: "bg-yellow-800",
        foreground: "text-yellow-800",
    },
    "light lime": {
        varName: "--color-lime-300",
        background: "bg-lime-300",
        foreground: "text-lime-300",
    },
    lime: {
        varName: "--color-lime-500",
        background: "bg-lime-500",
        foreground: "text-lime-500",
    },
    "dark lime": {
        varName: "--color-lime-800",
        background: "bg-lime-800",
        foreground: "text-lime-800",
    },
    "light green": {
        varName: "--color-green-300",
        background: "bg-green-300",
        foreground: "text-green-300",
    },
    green: {
        varName: "--color-green-500",
        background: "bg-green-500",
        foreground: "text-green-500",
    },
    "dark green": {
        varName: "--color-green-800",
        background: "bg-green-800",
        foreground: "text-green-800",
    },
    "light emerald": {
        varName: "--color-emerald-300",
        background: "bg-emerald-300",
        foreground: "text-emerald-300",
    },
    emerald: {
        varName: "--color-emerald-500",
        background: "bg-emerald-500",
        foreground: "text-emerald-500",
    },
    "dark emerald": {
        varName: "--color-emerald-800",
        background: "bg-emerald-800",
        foreground: "text-emerald-800",
    },
    "light teal": {
        varName: "--color-teal-300",
        background: "bg-teal-300",
        foreground: "text-teal-300",
    },
    teal: {
        varName: "--color-teal-500",
        background: "bg-teal-500",
        foreground: "text-teal-500",
    },
    "dark teal": {
        varName: "--color-teal-800",
        background: "bg-teal-800",
        foreground: "text-teal-800",
    },
    "light cyan": {
        varName: "--color-cyan-300",
        background: "bg-cyan-300",
        foreground: "text-cyan-300",
    },
    cyan: {
        varName: "--color-cyan-500",
        background: "bg-cyan-500",
        foreground: "text-cyan-500",
    },
    "dark cyan": {
        varName: "--color-cyan-800",
        background: "bg-cyan-800",
        foreground: "text-cyan-800",
    },
    "light sky": {
        varName: "--color-sky-300",
        background: "bg-sky-300",
        foreground: "text-sky-300",
    },
    sky: {
        varName: "--color-sky-500",
        background: "bg-sky-500",
        foreground: "text-sky-500",
    },
    "dark sky": {
        varName: "--color-sky-800",
        background: "bg-sky-800",
        foreground: "text-sky-800",
    },
    "light blue": {
        varName: "--color-blue-300",
        background: "bg-blue-300",
        foreground: "text-blue-300",
    },
    blue: {
        varName: "--color-blue-500",
        background: "bg-blue-500",
        foreground: "text-blue-500",
    },
    "dark blue": {
        varName: "--color-blue-800",
        background: "bg-blue-800",
        foreground: "text-blue-800",
    },
    "light indigo": {
        varName: "--color-indigo-300",
        background: "bg-indigo-300",
        foreground: "text-indigo-300",
    },
    indigo: {
        varName: "--color-indigo-500",
        background: "bg-indigo-500",
        foreground: "text-indigo-500",
    },
    "dark indigo": {
        varName: "--color-indigo-800",
        background: "bg-indigo-800",
        foreground: "text-indigo-800",
    },
    "light violet": {
        varName: "--color-violet-300",
        background: "bg-violet-300",
        foreground: "text-violet-300",
    },
    violet: {
        varName: "--color-violet-500",
        background: "bg-violet-500",
        foreground: "text-violet-500",
    },
    "dark violet": {
        varName: "--color-violet-800",
        background: "bg-violet-800",
        foreground: "text-violet-800",
    },
    "light purple": {
        varName: "--color-purple-300",
        background: "bg-purple-300",
        foreground: "text-purple-300",
    },
    purple: {
        varName: "--color-purple-500",
        background: "bg-purple-500",
        foreground: "text-purple-500",
    },
    "dark purple": {
        varName: "--color-purple-800",
        background: "bg-purple-800",
        foreground: "text-purple-800",
    },
    "light fuchsia": {
        varName: "--color-fuchsia-300",
        background: "bg-fuchsia-300",
        foreground: "text-fuchsia-300",
    },
    fuchsia: {
        varName: "--color-fuchsia-500",
        background: "bg-fuchsia-500",
        foreground: "text-fuchsia-500",
    },
    "dark fuchsia": {
        varName: "--color-fuchsia-800",
        background: "bg-fuchsia-800",
        foreground: "text-fuchsia-800",
    },
    "light pink": {
        varName: "--color-pink-300",
        background: "bg-pink-300",
        foreground: "text-pink-300",
    },
    pink: {
        varName: "--color-pink-500",
        background: "bg-pink-500",
        foreground: "text-pink-500",
    },
    "dark pink": {
        varName: "--color-pink-800",
        background: "bg-pink-800",
        foreground: "text-pink-800",
    },
};

export default colors;
