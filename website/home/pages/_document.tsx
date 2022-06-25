import { Html, Head, Main, NextScript } from "next/document";

const Document = () => (
    <Html>
        <Head>
            <link rel="preconnect" href="https://fonts.googleapis.com" />
            <link rel="preconnect" href="https://fonts.gstatic.com" crossOrigin="crossorigin" />
            <link
                rel="stylesheet"
                href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600&family=JetBrains+Mono:wght@300;600&display=swap"
            />
        </Head>

        <body>
            <Main />
            <NextScript />
        </body>
    </Html>
);

export default Document;
