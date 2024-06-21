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

export const signIn = async () => {
    const query = new URLSearchParams();
    query.append("redirect", window.location.href);
    window.location.href = `${window.location.origin}/login?${query.toString()}`;
};

export const signOut = async () => {
    const auth = getAuth();
    await auth.signOut();
};
