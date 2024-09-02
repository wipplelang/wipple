import { collection, doc, getDoc, getFirestore, setDoc } from "firebase/firestore";
import { getUser, pureConverter } from "../helpers";

export interface UserInfo {
    classroomCode?: string;
}

const userInfoConverter = pureConverter<UserInfo>();

export const getUserInfo = async (): Promise<UserInfo> => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to get user info");
    }

    const firestore = getFirestore();

    const playground = await getDoc(
        doc(firestore, "users", user.uid).withConverter(userInfoConverter),
    );

    return playground.data() ?? {};
};

export const updateUserInfo = async (userInfo: UserInfo) => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to update user info");
    }

    const firestore = getFirestore();

    await setDoc(doc(collection(firestore, "users"), user.uid), userInfo);
};
