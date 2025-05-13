export const Logo = (props: { title?: string }) => (
    <a href="/" className="flex flex-row items-center gap-2.5">
        <img src="/playground/images/logo.svg" alt="Wipple" className="w-8 h-8" />
        <p className="font-semibold">{props.title ?? "Wipple"}</p>
    </a>
);
