# Fractals

A little demo showing Lorentz attractor.

Made using ThreeJS and Elm.

# What to do ?

You can modify the attractor's parameters and see the result in your browser.


# Anaglyph

To use anaglyph rendering please check the "Anaglyph on/off"
The anaglyph rendering is done using the AnaglyphEffect library found at:

By włączyć renderowanie anaglifu należy zaznaczyć checkbox "Anaglyph on/off".
    http://threejs.org/examples/js/effects/AnaglyphEffect.js
This library was slighty modified. Functions 'getFocalLength" and 'setFocalLength' were added.



Renderowanie anaglifu jest zrobione za pomocą biblioteki AnaglyphEffect, pobranej ze strony:

Zawarty w katalogu AnaglyphEffect jest lekko zmodyfikowany - zostały dodane funkcje `getFocalLength`, `setFocalLength`. 

AnaglyphEffect korzysta z parametrów obiektu THREEJS.PerspectiveCamera, takich jak `near`, `far`, `aspect`, oraz `fov`.

AnaglyphEffect korzysta również z parametru `focalLenght`, który jest niezależny od użytej kamery.
Za pomocą funkcji `setFocalLength` można zmienić ten parametr.
