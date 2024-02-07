import { GoogleAuthProvider, getAuth, signInAnonymously, signInWithPopup } from "firebase/auth";

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

export const signInWithGoogle = async () => {
    const provider = new GoogleAuthProvider();
    provider.addScope("https://www.googleapis.com/auth/userinfo.profile");

    const auth = getAuth();

    try {
        const result = await signInWithPopup(auth, provider);
        return result.user;
    } catch (error) {
        console.error(error);
        return undefined;
    }
};

export const signOut = async () => {
    const auth = getAuth();
    await auth.signOut();
};
