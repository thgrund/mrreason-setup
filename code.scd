
var eventmapper = { |result, soundName|
	var drumBank, prefix, finalSoundName;
	var event = ();
	var pan = (currentEnvironment.at(\pan));

	event.putAll((\type: \dirt, \dirt: ~dirt), currentEnvironment);

	//currentEnvironment.postln;

	if (event.at('n').isNil, {event.add(\n -> 0.0)});
	if (event.at('drumBank').isNil, {drumBank = 0}, {drumBank = event.at('drumBank')});


	// plays a samplebank with summer as a prefix.
	// i.e. summerbd, summersn or summerhh
	if (drumBank == 0, {finalSoundName = 'summer' ++ soundName});
	if (drumBank == 1, {finalSoundName = 'kontakt' ++ soundName})
    // Add more drumBanks to your needs

	event.add(\pan -> ((pan + 1) / 2));
	event.add(\type-> \dirt);
	event.add(\dirt-> ~dirt);
	event.add(\s -> (finalSoundName).asSymbol);
	event.add(\amp -> ~gain);
	event.add(\freq -> (event.at('n') + 60).midicps );
	event.add(\freq -> (event.at('n') + 60).midicps );
	result.putAll(event);

	result

};


~dirt.soundLibrary.addSynth( \bd, (play: { var result =  (); eventmapper.value(result, 'bd'); result.play;}));
~dirt.soundLibrary.addSynth( \sn, (play: { var result =  (); eventmapper.value(result, 'sn'); result.play;}));
~dirt.soundLibrary.addSynth( \tom, (play: { var result =  (); eventmapper.value(result, 'tom'); result.play;}));
~dirt.soundLibrary.addSynth( \hh, (play: { var result =  (); eventmapper.value(result, 'hh'); result.play;}));
~dirt.soundLibrary.addSynth( \hho, (play: { var result =  (); eventmapper.value(result, 'hho'); result.play;}));
~dirt.soundLibrary.addSynth( \cr, (play: { var result =  (); eventmapper.value(result, 'cr'); result.play;}));
~dirt.soundLibrary.addSynth( \rd, (play: { var result =  (); eventmapper.value(result, 'rd'); result.play;}));
~dirt.soundLibrary.addSynth( \ch, (play: { var result =  (); eventmapper.value(result, 'ch'); result.play;}));
~dirt.soundLibrary.addSynth( \mi, (play: { var result =  (); eventmapper.value(result, 'mi'); result.play;}));
~dirt.soundLibrary.addSynth( \co, (play: { var result =  (); eventmapper.value(result, 'co'); result.play;}));
~dirt.soundLibrary.addSynth( \bl, (play: { var result =  (); eventmapper.value(result, 'bl'); result.play;}));
~dirt.soundLibrary.addSynth( \log, (play: { var result =  (); eventmapper.value(result, 'log'); result.play;}));
~dirt.soundLibrary.addSynth( \sc, (play: { var result =  (); eventmapper.value(result, 'sc'); result.play;}));
~dirt.soundLibrary.addSynth( \cl, (play: { var result =  (); eventmapper.value(result, 'cl'); result.play;}));
