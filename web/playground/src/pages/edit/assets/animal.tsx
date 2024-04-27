import { useMemo } from "react";
import { Tooltip } from "../../../components";

export const animals = [
    "ant",
    "badger",
    "bat",
    "bear",
    "beaver",
    "beetle",
    "bird",
    "bison",
    "blowfish",
    "boar",
    "bug",
    "butterfly",
    "camel",
    "cat",
    "chicken",
    "chipmunk",
    "cockroach",
    "coral",
    "cow",
    "crab",
    "cricket",
    "crocodile",
    "deer",
    "dodo",
    "dog",
    "dolphin",
    "dove",
    "dragon",
    "duck",
    "eagle",
    "elephant",
    "ewe",
    "feather",
    "fish",
    "flamingo",
    "fly",
    "fox",
    "frog",
    "giraffe",
    "goat",
    "gorilla",
    "hamster",
    "hedgehog",
    "hippopotamus",
    "honeybee",
    "horse",
    "kangaroo",
    "koala",
    "leopard",
    "lion",
    "lizard",
    "llama",
    "lobster",
    "mammoth",
    "microbe",
    "monkey",
    "mosquito",
    "mouse",
    "octopus",
    "orangutan",
    "otter",
    "owl",
    "ox",
    "oyster",
    "panda",
    "parrot",
    "peacock",
    "penguin",
    "pig",
    "poodle",
    "rabbit",
    "raccoon",
    "ram",
    "rat",
    "rhinoceros",
    "rooster",
    "sauropod",
    "scorpion",
    "seal",
    "shark",
    "shrimp",
    "skunk",
    "sloth",
    "snail",
    "snake",
    "spider",
    "squid",
    "swan",
    "t_rex",
    "tiger",
    "turkey",
    "turtle",
    "unicorn",
    "whale",
    "wolf",
    "worm",
    "zebra",
];

export const animalImageUrl = (animal: string) => `/playground/images/animals/${animal}.svg`;

export const AnimalAsset = (props: { animal: string; disabled?: boolean; onClick: () => void }) => {
    const description = useMemo(() => props.animal.replace("_", "_"), [props.animal]);
    const imageUrl = useMemo(() => animalImageUrl(props.animal), [props.animal]);

    return (
        <div className="inline-block align-text-bottom rounded-md border-2 border-gray-100 dark:border-gray-800 overflow-clip hover:scale-110 transition-transform">
            <Tooltip
                disabled={props.disabled}
                description={<span className="capitalize">{description}</span>}
                onClick={props.onClick}
            >
                <img className="w-4 h-4" src={imageUrl} />
            </Tooltip>
        </div>
    );
};
