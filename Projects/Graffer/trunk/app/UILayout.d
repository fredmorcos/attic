/*
	This file is part of Grafer.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Grafer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Grafer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Grafer.  If not, see <http://www.gnu.org/licenses/>.
*/

module app.UILayout;

private import
	graph.alg.ForceLayout, 
	gtk.SpinButton, 
	gtk.Button, 
	gtk.Label, 
	gtk.CheckButton, 
	gtk.ComboBox, 
	gtk.HBox, 
	gtk.VBox, 
	gtk.Expander,
	gtk.ToggleButton;

class UILayout: Expander {
protected:
	ComboBox	algCombo;
	SpinButton	eConstSpin,
				sConstSpin,
				dampSpin,
				timestepSpin,
				sNLSpin,
				mtSpin;
	CheckButton	animateCheck,
				wallCheck,
				sNLCheck,
				mtCheck;
	Button		runButton,
				stopButton;
	HBox		buttonsBox,
				algBox,
				sNLBox,
				sBox,
				eBox,
				dBox,
				tsBox,
				mtBox;
	VBox		box;

public:
	this() {
		super("Layout");

		with (algCombo = new ComboBox()) {
			appendText("Simple");
			appendText("Time");
			appendText("Barnes-Hut");
			setActive(0);
		}

		with (algBox = new HBox(false, 5)) {
			packStart(new Label("Algorithm"), false, false, 0);
			packStart(algCombo, true, true, 0);
		}

		with (eConstSpin = new SpinButton(0, 10000, 10)) {
			eConstSpin.setValue(100);
		}

		with (eBox = new HBox(false, 5)) {
			packStart(new Label("Repulsion Constant"), false, false, 0);
			packStart(eConstSpin, true, true, 0);
		}

		with (sConstSpin = new SpinButton(0, 10000, 10)) {
			sConstSpin.setValue(10);
		}

		with (sBox = new HBox(false, 5)) {
			packStart(new Label("Attraction Constant"), false, false, 0);
			packStart(sConstSpin, true, true, 0);
		}

		with (mtCheck = new CheckButton("Threads")) {
			addOnToggled(&mtCheckToggled);
		}

		with (mtSpin = new SpinButton(2, 10, 1)) {
			setValue(2);
			setSensitive(false);
		}

		with (mtBox = new HBox(false, 5)) {
			packStart(mtCheck, false, false, 0);
			packStart(mtSpin, true, true, 0);
		}

		with (sNLCheck = new CheckButton("Edge Length")) {
			addOnToggled(&sNLCheckToggled);
		}

		with (sNLSpin = new SpinButton(0, 1000, 1)) {
			setValue(20);
			setSensitive(false);
		}

		with (sNLBox = new HBox(false, 5)) {
			packStart(sNLCheck, false, false, 0);
			packStart(sNLSpin, true, true, 0);
		}

		with (dampSpin = new SpinButton(0, 10, 1)) {
			setValue(9);
		}

		with (dBox = new HBox(false, 5)) {
			packStart(new Label("Damping"), false, false, 0);
			packStart(dampSpin, true, true, 0);
		}

		with (timestepSpin = new SpinButton(0, 100, 1)) {
			setValue(4);
		}

		with (tsBox = new HBox(false, 5)) {
			packStart(new Label("Timestep"), false, false, 0);
			packStart(timestepSpin, true, true, 0);
		}

		with (animateCheck = new CheckButton("Animate")) {
		}

		with (wallCheck = new CheckButton("Wall")) {
		}

		with (runButton = new Button(StockID.EXECUTE, false)) {
		}

		with (stopButton = new Button(StockID.STOP, false)) {
			setSensitive(false);
		}

		with (buttonsBox = new HBox(false, 5)) {
			packStart(runButton, true, true, 0);
			packStart(stopButton, true, true, 0);
		}

		with (box = new VBox(false, 5)) {
			packStart(algBox, false, false, 0);
			packStart(eBox, false, false, 0);
			packStart(sBox, false, false, 0);
			packStart(sNLBox, false, false, 0);
			packStart(dBox, false, false, 0);
			packStart(tsBox, false, false, 0);
			packStart(mtBox, false, false, 0);
			packStart(animateCheck, false, false, 0);
			packStart(wallCheck, false, false, 0);
			packStart(buttonsBox, false, false, 0);
		}

		add(box);
	}

	~this() {
		delete algCombo;
		delete eConstSpin;
		delete sConstSpin;
		delete dampSpin;
		delete timestepSpin;
		delete animateCheck;
		delete wallCheck;
		delete mtCheck;
		delete mtSpin;
		delete mtBox;
		delete sNLCheck;
		delete sNLSpin;
		delete algBox;
		delete sNLBox;
		delete sBox;
		delete eBox;
		delete dBox;
		delete tsBox;
		delete buttonsBox;
	}

	void mtCheckToggled(ToggleButton button) {
		mtSpin.setSensitive(button.getActive);
	}

	void sNLCheckToggled(ToggleButton button) {
		sNLSpin.setSensitive(button.getActive);
	}

	bool getThreadsEnable () {
		return cast(bool) mtCheck.getActive;
	}

	int getThreadsNum () {
		return mtSpin.getValueAsInt;
	}

	CheckButton	getWallCheck() {
		return wallCheck;
	}

	CheckButton getAnimateCheck() {
		return animateCheck;
	}

	Button getRunButton() {
		return runButton;
	}

	Button getStopButton() {
		return stopButton;
	}

	bool getEnableAnimation() {
		return cast(bool) animateCheck.getActive;
	}

	bool getEnableWall() {
		return cast(bool) wallCheck.getActive;
	}

	int getElectricConst() {
		return eConstSpin.getValueAsInt;
	}

	int getSpringConst() {
		return sConstSpin.getValueAsInt;
	}

	int getDamping() {
		return dampSpin.getValueAsInt();
	}

	int getTimestep() {
		return timestepSpin.getValueAsInt();
	}

	int getSpringNL() {
		return sNLSpin.getValueAsInt();
	}

	bool getEnableNL() {
		return cast(bool) sNLCheck.getActive();
	}

	ForceLayoutType getAlgorithmType() {
		switch(algCombo.getActiveText()) {
			case "Time":			return ForceLayoutType.TIME;
			case "Barnes-Hut":		return ForceLayoutType.BH;
			default:				return ForceLayoutType.SIMPLE;
		}
	}
}

