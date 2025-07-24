export const trackWidgetValue = (host: HTMLElement, getProps: () => Record<string, any>) => {
    let firstMount = true;
    $effect(() => {
        getProps(); // track the props

        if (firstMount) {
            firstMount = false;
            return;
        }

        host.dispatchEvent(new CustomEvent("change"));
    });
};
