import { Drawer, IconButton } from "@mui/material";
import { GetStaticPaths, GetStaticProps, NextPage } from "next";
import fs from "fs";
import path from "path";
import { ParsedUrlQuery } from "querystring";
import { useEffect, useState } from "react";
import sections, { type Page } from "../../../docs";
import CodePreview from "../../../components/CodePreview";
import { serialize as serializeMDX } from "next-mdx-remote/serialize";
import { MDXRemote } from "next-mdx-remote";
import MenuIcon from "@mui/icons-material/Menu";
import ArrowBackIcon from "@mui/icons-material/ArrowBack";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import remarkMath from "remark-math";
import remarkGfm from "remark-gfm";
import rehypeKatex from "rehype-katex";
import remarkSmartypants from "remark-smartypants";

interface Props {
    page: any;
    path: string;
    previous: Page | null;
    next: Page | null;
}

interface Params extends ParsedUrlQuery {
    section: string;
    page: string;
}

const Page: NextPage<Props> = (props) => {
    const [windowWidth, setWindowWidth] = useState(0);

    useEffect(() => {
        setWindowWidth(window.innerWidth);

        window.addEventListener("resize", () => {
            setWindowWidth(window.innerWidth);
        });
    }, []);

    const showSidebarInline = windowWidth > 768;

    const [drawerIsOpen, setDrawerOpen] = useState(false);

    return (
        <div>
            <Drawer
                variant={showSidebarInline ? "permanent" : "temporary"}
                open={drawerIsOpen}
                onClose={() => setDrawerOpen(false)}
            >
                <div className="w-72 h-full p-5">
                    {sections.map((section) => (
                        <div key={section.path} className="mb-6">
                            <h2 className="text-sm uppercase text-gray-500">{section.title}</h2>

                            <ul>
                                {section.pages.map((page) => (
                                    <li
                                        key={page.path}
                                        className={`mt-2 ${
                                            path.parse(page.path).base == props.path
                                                ? "font-bold"
                                                : ""
                                        }`}
                                    >
                                        <a href={`/docs/${page.path}`}>{page.title}</a>
                                    </li>
                                ))}
                            </ul>
                        </div>
                    ))}
                </div>
            </Drawer>

            <div className={`${showSidebarInline ? "pl-72" : ""} max-w-7xl mx-auto`}>
                <div className="flex items-center">
                    {!showSidebarInline && (
                        <IconButton className="ml-2.5" onClick={() => setDrawerOpen(!drawerIsOpen)}>
                            <MenuIcon />
                        </IconButton>
                    )}

                    <div className="flex items-center w-full p-5">
                        <div className="flex items-center gap-4">
                            <a href="/">
                                <img src="/logo.svg" className="w-10 h-10" />
                            </a>

                            {showSidebarInline && (
                                <p className="font-bold text-lg">Wipple Documentation</p>
                            )}
                        </div>

                        <div className="flex-grow" />

                        <div className="flex gap-4 text-gray-500">
                            <a href="/">Home</a>

                            <a target="_blank" href="https://playground.wipple.gramer.dev">
                                Playground
                            </a>

                            <a target="_blank" href="https://github.com/wipplelang/wipple">
                                GitHub
                            </a>
                        </div>
                    </div>
                </div>

                <div className="prose prose-lg prose-sky m-5 max-w-full">
                    <MDXRemote components={{ pre: CodePreview }} {...props.page} />
                </div>

                <div className="flex m-5 gap-4">
                    <div className="flex-1">
                        {props.previous && (
                            <a href={`/docs/${props.previous.path}`}>
                                <div
                                    className="p-4 rounded-md border-sky-100 text-sky-500"
                                    style={{ borderWidth: 1 }}
                                >
                                    <div>
                                        <ArrowBackIcon className="-ml-1 mb-2" />
                                    </div>
                                    {props.previous.title}
                                </div>
                            </a>
                        )}
                    </div>

                    <div className="flex-1">
                        {props.next && (
                            <a href={`/docs/${props.next.path}`}>
                                <div
                                    className="text-right p-4 rounded-md border-sky-100 text-sky-500"
                                    style={{ borderWidth: 1 }}
                                >
                                    <div className="ml-auto">
                                        <ArrowForwardIcon className="-mr-1 mb-2" />
                                    </div>
                                    {props.next.title}
                                </div>
                            </a>
                        )}
                    </div>
                </div>

                <div className="mb-4 text-center text-gray-400">
                    Made by{" "}
                    <a target="_blank" href="https://gramer.dev" className="text-gray-500">
                        Wilson Gramer
                    </a>
                </div>
            </div>
        </div>
    );
};

export default Page;

export const getStaticProps: GetStaticProps<Props, Params> = async ({ params }) => {
    const sectionIndex = sections.findIndex((s) => s.path == params!.section);
    const section = sections[sectionIndex];
    const previousSection = sections[sectionIndex - 1];
    const nextSection = sections[sectionIndex + 1];
    const pageIndex = section.pages.findIndex((p) => path.parse(p.path).base == params!.page);
    const page = section.pages[pageIndex];
    const previous =
        section.pages[pageIndex - 1] ??
        previousSection?.pages[previousSection.pages.length - 1] ??
        null;
    const next = section.pages[pageIndex + 1] ?? nextSection?.pages[0] ?? null;

    const markdown = fs.readFileSync(
        path.join(process.cwd(), "docs", "sections", `${page.path}.mdx`),
        "utf8"
    );

    const mdx = await serializeMDX(markdown, {
        mdxOptions: {
            remarkPlugins: [remarkMath, remarkGfm, remarkSmartypants],
            rehypePlugins: [rehypeKatex],
        },
    });

    return {
        props: {
            page: mdx,
            path: path.parse(page.path).base,
            previous,
            next,
        },
    };
};

export const getStaticPaths: GetStaticPaths<Params> = () => {
    return {
        paths: sections.flatMap((section) =>
            section.pages.map((page) => ({
                params: {
                    section: section.path,
                    page: path.parse(page.path).base,
                },
            }))
        ),
        fallback: false,
    };
};
