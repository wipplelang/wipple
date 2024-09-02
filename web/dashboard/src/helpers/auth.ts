import { getAuth, signInAnonymously } from "firebase/auth";

export const getUser = async () => {
    const auth = getAuth();
    await auth.authStateReady();

    if (auth.currentUser) {
        return auth.currentUser;
    } else if (import.meta.env.DEV) {
        console.warn("In dev mode; signing in anonymously...");

        const userCredential = await signInAnonymously(auth);
        console.warn("User ID:", userCredential.user.uid);

        return userCredential.user;
    }
};
