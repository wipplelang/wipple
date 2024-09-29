import { useNavigate } from "react-router-dom";
import { UserButton } from "./user";

export const Navbar = (props: { title: React.ReactNode }) => {
    const navigate = useNavigate();

    return (
        <div className="flex flex-row items-center justify-between px-4 h-20">
            <div className="flex flex-row items-center gap-4">
                <button onClick={() => navigate(import.meta.env.BASE_URL)}>
                    <img src="/playground/images/logo.svg" alt="Wipple" className="w-10 h-10" />
                </button>

                {props.title}
            </div>

            <UserButton />
        </div>
    );
};
