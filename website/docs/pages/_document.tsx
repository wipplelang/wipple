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
            <link
                rel="stylesheet"
                href="https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.css"
                integrity="sha384-MlJdn/WNKDGXveldHDdyRP1R4CTHr3FeuDNfhsLPYrq2t0UBkUdK2jyTnXPEK1NQ"
                crossOrigin="anonymous"
            ></link>
        </Head>

        <body>
            <Main />
            <NextScript />
        </body>
    </Html>
);

export default Document;
