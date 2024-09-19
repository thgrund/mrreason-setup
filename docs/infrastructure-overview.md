# Infrastructure overview

This is a screenshot from my editor and it shows what the audience sees. The informations I share are:

- Artist name
- Song name
- Number of segments and active segment
- Audio visualizer
- Code (what you see is what you hear)

The changes are not officially published, but can be found here: https://github.com/thgrund/pulsar-tidalcycles/tree/mrreason-setup

## ![Editor](../asset/images/Editor.png)

## Message flow between components

Uses partly non published code from https://github.com/thgrund/ep (private)

This is an overview to show, how every piece in my setup exchanges states and informations:

![MrReasonSetup.drawio](../asset/images/MrReasonSetup.drawio.svg)

## OSC Messages

### SuperCollider (Remote.scd)

```c++
atomOSC.sendMsg("/pulsar/eval", "search", value.segment[0]);
pulsarOSC.sendMsg("/pulsar/gradient", "gradient", "orangered");

hydaOSC.sendMsg("/hydra/video", "source", videoSource);

tidalOSC.sendMsg("/ctrl", "duration", value.segment[2]);
tidalOSC.sendMsg("/ctrl", "offset", (cycle % value.segment[2]).neg + offset);
tidalOSC.sendMsg("/ctrl", parsedJSON.songName, value.segment[3]);
tidalOSC.sendMsg("/ctrl", "presetFile", "~");

superDirtOSC.sendMsg("/SuperDirtMixer/loadPreset", value.segment[4]);
superDirtOSC.sendMsg("/SuperDirtMixer/tidalvstPreset", \pliniRhytm, "/Users/mrreason/Development/SuperCollider/TidalVST/presets/pliniX/forestPliniRhytm.vstpreset");
superDirtOSC.sendMsg("/SuperDirtMixer/midiControlButton", num - 100);


```

### TidalCycles

```c++
"/ctrl" "duration" cycleNumber
"/ctrl" "offset" offeset
"/ctrl" parsedJSON.songName pattern | ur pattern like "1@4 2@1 3@8"
"/ctrl" "presetFile" "~"
```

### Tidalcycles Package

```c++
"/pulsar/buttons"  "enabled" size "color" hexColor
"/pulsar/button": buttonIndex
"/pulsar/headlines": "songTitle" songTitle
"/pulsar/audio": AudioDevice Name (MediaDevice HTML5 Name)
"/pulsar/gradient": gradientColor (orangered | classic)
"/pulsar/remote-control/index"
"/pulsar/remote-control/preset"
"/pulsar/eval" "search" search term
```

### SuperDirtMixer

```c++
"/SuperDirtMixer/loadPreset": presetFile
"/SuperDirtMixer/pan": orbitIndex value
"/SuperDirtMixer/masterGain": orbitIndex value
"/SuperDirtMixer/reverb": orbitIndex value
"/SuperDirtMixer/tidalvstPreset": fxName presetPath
"/SuperDirtMixer/midiControlButton": midiControlButtonIndex
```

### Hydra

```c++
'/hydra/video': video path (relativ)
```
