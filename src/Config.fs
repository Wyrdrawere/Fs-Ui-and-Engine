namespace Engine

//todo: make into object, thats supposed to be created in maingame/app. then move defaultFont here
module Config =
    //todo: remove game specific stuff from here 
    let screenWidth = 800
    let screenHeight = 600
    let maxATB = 3
    let atbCapacity = 100.0f
    //todo: probably not needed anymore
    let inputDelay = 0.25f