import { getAuth, signInAnonymously } from "firebase/auth";

export const getUser = async () => {
    const auth = getAuth();
    await auth.authStateReady();

    return auth.currentUser;
};

export const signInAsGuest = async () => {
    const auth = getAuth();
    const result = await signInAnonymously(auth);
    return result.user;
};
