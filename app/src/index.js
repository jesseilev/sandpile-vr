var Elm = require( './Main' );
Elm.Main.embed(document.body);

Elm.ports.enterVr.subscribe(function () {
    console.log("you clicked it!");

    var scene = document.querySelector('a-scene');
    if (scene.hasLoaded) {
       scene.enterVR();
    } else {
       scene.addEventListener('loaded', function () {
           setTimeout(function () {
               scene.enterVR();
           }, 1000);
       });
    };
});
