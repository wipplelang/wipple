import { initializeApp } from "firebase/app";
import { GoogleAuthProvider, getAuth, getRedirectResult, signInWithRedirect } from "firebase/auth";

initializeApp({
    apiKey: import.meta.env.VITE_FIREBASE_API_KEY,
    authDomain: import.meta.env.VITE_FIREBASE_AUTH_DOMAIN,
    projectId: import.meta.env.VITE_FIREBASE_PROJECT_ID,
    storageBucket: import.meta.env.VITE_FIREBASE_STORAGE_BUCKET,
    messagingSenderId: import.meta.env.VITE_FIREBASE_MESSAGING_SENDER_ID,
    appId: import.meta.env.VITE_FIREBASE_APP_ID,
    measurementId: import.meta.env.VITE_FIREBASE_MEASUREMENT_ID,
});

(async () => {
    const auth = getAuth();

    const redirectResult = await getRedirectResult(auth);

    console.log("redirectResult:", redirectResult);

    if (redirectResult) {
        const redirect = new URLSearchParams(window.location.search).get("redirect");
        window.location.href = redirect || "/";
    } else {
        const provider = new GoogleAuthProvider();
        provider.addScope("https://www.googleapis.com/auth/userinfo.profile");

        try {
            await signInWithRedirect(auth, provider);
        } catch (error) {
            console.error(error);
        }
    }
})();
