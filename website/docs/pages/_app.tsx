import "../styles/globals.css";
import "../styles/hljs-theme.css";
import { AppProps } from "next/app";
import hljs from "highlight.js";

import typescriptLanguage from "highlight.js/lib/languages/typescript";
hljs.registerLanguage("typescript", typescriptLanguage);

import shellLanguage from "highlight.js/lib/languages/shell";
hljs.registerLanguage("shell", shellLanguage);

import wippleLanguage from "../languages/wipple";
hljs.registerLanguage("wipple", wippleLanguage);

const App = ({ Component, pageProps }: AppProps) => <Component {...pageProps} />;

export default App;
