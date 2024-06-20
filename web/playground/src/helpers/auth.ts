import {
    GoogleAuthProvider,
    getAuth,
    getRedirectResult,
    signInAnonymously,
    signInWithRedirect,
} from "firebase/auth";

export const getUser = async () => {
    const auth = getAuth();
    await auth.authStateReady();

    const redirectResult = await getRedirectResult(auth);
    if (redirectResult) {
        auth.updateCurrentUser(redirectResult.user);
    }

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
        await signInWithRedirect(auth, provider);
    } catch (error) {
        console.error(error);
        return undefined;
    }
};

export const signOut = async () => {
    const auth = getAuth();
    await auth.signOut();
};
