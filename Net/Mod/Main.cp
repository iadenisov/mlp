MODULE NetMain;
	(**
		(c) Ivan Denisov
	**)

	IMPORT DiaPlot, Dialog, Files, Math, Win := WinApi, Ports, Properties, Services, StdCmds, Log, Stores, Views, Models, Kernel, Windows, Controllers,
	NetService, NetPlotMap, NetModel, NetRandom;

	TYPE
		Error = RECORD
			pos: INTEGER;
			line: ARRAY 50000 OF REAL;
		END;
		Teacher = POINTER TO RECORD (Services.Action)
			single: BOOLEAN;
			m: POINTER TO ARRAY OF INTEGER;
		END;
		View = POINTER TO RECORD (Views.View)
			l: INTEGER
		END;
		Msg = RECORD (Models.Message) END;
		
	CONST
		responce = 100; (* Cycles without break *)
	
	VAR
		timeout*, times*, layers*, neurons*, tspeed*, repeate*, nnn*, counter-, try-,
		statStep-, l1*, l2*, w1*, w2*, inputs-, outputs-: INTEGER;
		time-, wamp*, kickamp*: REAL;
		speed*, errormax*, aerror-, x1*, y1*, x2*, y2*: REAL;
		mix*, showdinamics*, timeoutReset*, nnready, teachtest, isKick,
		statstop, keep*: BOOLEAN;
		tmp, input, output : POINTER TO ARRAY OF ARRAY OF REAL;
		inputFile-, outputFile-: ARRAY 256 OF CHAR;
		temp: Error;
		backupnet, hotnet: NetModel.NeuroNet;
		netteacher: Teacher;
		a, b: DiaPlot.Axes;
		rand: NetRandom.RandomGenerator;
		time1: LONGINT;
		view: View;
	
	PROCEDURE UpdateLang*;
	BEGIN
		Dialog.MapString("#Net:choose file", inputFile);
		Dialog.MapString("#Net:choose file", outputFile);
	END UpdateLang;
	
	PROCEDURE GetWindow (name: ARRAY OF CHAR): Windows.Window;
		VAR win, winTmp: Windows.Window; title: Views.Title;
	BEGIN
		win := Windows.dir.First();
		winTmp := NIL;
		WHILE (win # NIL) & (winTmp = NIL) DO
			win.GetTitle(title);
			IF title$ = name$ THEN
				winTmp := win
			END;
			win := Windows.dir.Next(win)
		END;
		RETURN winTmp
	END GetWindow;

	PROCEDURE ProcessMessages;
		VAR msg: Win.MSG; res: INTEGER;
	BEGIN
		WHILE Win.PeekMessage(msg, 0, 0, 0, Win.PM_REMOVE) # 0 DO
			res := Win.TranslateMessage(msg); res := Win.DispatchMessageA(msg)
	END END ProcessMessages;
	
	PROCEDURE I (re: REAL): INTEGER; BEGIN RETURN SHORT(ENTIER(re)) END I;

	PROCEDURE Mixer* (len: INTEGER): POINTER TO ARRAY OF INTEGER;
		VAR
			i, c, tmp, a1, a2: INTEGER; temp: POINTER TO ARRAY OF INTEGER;
	BEGIN
		NEW(temp, len);
		FOR i:=0 TO len - 1 DO temp[i] := i END;
		FOR c := 0 TO len*len DO
			a1 := SHORT(ENTIER(rand.Uniform() * len));
			a2 := SHORT(ENTIER(rand.Uniform() * len));
			tmp := temp[a1];
			temp[a1] := temp[a2];
			temp[a2] := tmp;
		END;
		RETURN temp
	END Mixer;
	
	PROCEDURE Max* (a: ARRAY OF REAL): REAL;
		VAR i: INTEGER; max: REAL;
	BEGIN
		max := a[0]; i := 1;
		WHILE i < LEN(a) DO IF a[i] > max THEN max := a[i]; END; INC(i) END;
		RETURN max
	END Max;

	PROCEDURE Min* (a: ARRAY OF REAL): REAL;
		VAR i: INTEGER; min: REAL;
	BEGIN
		min := a[0]; i := 1;
		WHILE i < LEN(a) DO IF a[i] < min THEN min := a[i] END; INC(i) END;
		RETURN min
	END Min;

	PROCEDURE ShowNN* ();
	VAR mess: ARRAY 128 OF CHAR;
	BEGIN
		IF view = NIL THEN
			NEW(view);
		END;
		view.l := - 1; (* show all layers *)
		Dialog.MapString("#Net:ANN representation", mess);
		Views.OpenAux(view, mess$);
	END ShowNN;

	PROCEDURE (a: View) Restore (f: Views.Frame; fl, ft, fr, fb: INTEGER);
		VAR x, y, r, midle, i, j, k, dens, vdens, xt, yt, col,
			width, height, top: INTEGER; porog, ttt: REAL;
	BEGIN
		IF hotnet # NIL THEN
			a.context.GetSize(width, height);
			dens := width DIV f.dot DIV hotnet.neurons;
			fr := width;
			vdens := height DIV f.dot DIV hotnet.layers;
			r := dens DIV 3 + 1;
			IF r > 10 THEN r := 10 END;
			porog := 0.1;
			
			(* первый ряд *)
			midle := hotnet.neurons * dens DIV 2;
			top := 30;
			y := top;
			FOR i := 0 TO hotnet.input - 1 DO
				x := midle + (i + 1) * dens - hotnet.input * dens DIV 2 - dens DIV 2;
				yt := y + vdens;
				IF (a.l < 0) OR (a.l = 1) THEN
					FOR k := 0 TO hotnet.neurons - 1 DO
						xt := (k + 1) * dens - dens DIV 2;
						ttt := hotnet.w[0, i, k];
						IF ttt < 0 THEN col := Ports.blue ELSE col := Ports.red END;
						IF ABS(ttt) > porog THEN
							
							f.DrawLine(x * f.dot, y * f.dot, xt * f.dot, yt * f.dot, I(ABS(ttt) * r / 2 * f.dot), col)
						END
					END
				END
			END;
			(* Скрытые слои *)
			FOR j := 1 TO hotnet.layers - 3 DO
				y := y + vdens;
				IF (a.l < 0) OR (a.l = j + 1) THEN
					FOR i := 0 TO hotnet.neurons - 1 DO
						x := (i + 1) * dens - dens DIV 2;
						yt := y + vdens;
						FOR k := 0 TO hotnet.neurons - 1 DO
							xt := (k + 1) * dens - dens DIV 2;
							ttt := hotnet.w[j, i, k];
							IF ttt < 0 THEN col := Ports.blue ELSE col := Ports.red END;
							IF ABS(ttt) > porog THEN
								f.DrawLine(x * f.dot, y * f.dot, xt * f.dot, yt * f.dot, I(ABS(ttt) * r / 2 * f.dot), col)
							END
						END
					END
				END
			END;
			(* Последний слой *)
			y := y + vdens;
			IF (a.l < 0) OR (a.l = hotnet.layers - 1) THEN
				FOR i := 0 TO hotnet.neurons - 1 DO
					x := (i + 1) * dens - dens DIV 2;
					yt := y + vdens;
					FOR k := 0 TO hotnet.output - 1 DO
						xt := midle + (k + 1) * dens - hotnet.output * dens DIV 2 - dens DIV 2;
						ttt := hotnet.w[hotnet.layers - 2, i, k];
						IF ttt < 0 THEN col := Ports.blue ELSE col := Ports.red END;
						IF ABS(ttt) > porog THEN
							f.DrawLine(x * f.dot, y * f.dot, xt * f.dot, yt * f.dot, I(ABS(ttt) * r / 2 * f.dot), col)
						END
					END
				END
			END;
			
			midle := hotnet.neurons * dens DIV 2;
			top := 30;
			y := top;
		
			FOR i := 1 TO hotnet.input DO
				x := midle + i * dens - hotnet.input * dens DIV 2 - dens DIV 2;
				yt := y + vdens;
				f.DrawOval(I(x - r) * f.dot, I(y - r) * f.dot, I(x + r) * f.dot, I(y + r) * f.dot, - 1, Ports.green)
			END;
			FOR j := 1 TO hotnet.layers - 2 DO
				y := y + vdens;
				IF j # hotnet.layers - 2 THEN
					FOR i := 1 TO hotnet.neurons DO
						x := i * dens - dens DIV 2;
						yt := y + vdens;
						f.DrawOval(I(x - r) * f.dot, I(y - r) * f.dot, I(x + r) * f.dot, I(y + r) * f.dot, - 1, Ports.black)
					END
				ELSE
					FOR i := 1 TO hotnet.neurons DO
						x := i * dens - dens DIV 2;
						yt := y + vdens;
						f.DrawOval(I(x - r) * f.dot, I(y - r) * f.dot, I(x + r) * f.dot, I(y + r) * f.dot, - 1, Ports.black)
					END
				END
			END;
			y := y + vdens;
			FOR i := 1 TO hotnet.output DO
				x := midle + i * dens - hotnet.output * dens DIV 2 - dens DIV 2;
				f.DrawOval(I(x - r) * f.dot, I(y - r) * f.dot, I(x + r) * f.dot, I(y + r) * f.dot, - 1, Ports.green)
			END
		END
	END Restore;
	
	PROCEDURE (v: View) CopyFromSimpleView- (source: Views.View);
	BEGIN
		WITH source: View DO
			v.l := source.l
		END
	END CopyFromSimpleView;

	PROCEDURE (v: View) HandlePropMsg (VAR msg: Properties.Message);
		CONST w = 90 * Ports.mm; h = 90 * Ports.mm;
	BEGIN
		WITH msg: Properties.SizePref DO
			IF msg.w = Views.undefined THEN
				msg.w := w; msg.h := h
			END
		| msg: Properties.ResizePref DO
			msg.horFitToWin := TRUE; msg.verFitToWin := TRUE
	END END HandlePropMsg;
	
	PROCEDURE (v: View) HandleCtrlMsg  (f: Views.Frame; VAR msg: Views.CtrlMessage; VAR focus: Views.View);
	BEGIN
		WITH msg: Controllers.WheelMsg DO
			IF msg.op = Controllers.incLine THEN
				IF v.l < layers + 1 THEN INC(v.l) END
			ELSIF msg.op = Controllers.decLine THEN
				IF v.l > - 1 THEN DEC(v.l) END
			END;
			Views.Update(v, Views.keepFrames)
		ELSE END
	END HandleCtrlMsg;

	PROCEDURE (v: View) Externalize- (VAR wr: Stores.Writer);
	BEGIN
		Stores.Join(v, hotnet);
		wr.WriteInt(v.l);
		wr.WriteStore(hotnet)
	END Externalize;
	
	
	PROCEDURE (v: View) Internalize- (VAR rd: Stores.Reader);
	VAR s: Stores.Store;
	BEGIN
		rd.ReadInt(v.l);
		rd.ReadStore(s);
		WITH s : NetModel.NeuroNet DO
			hotnet := s;
			layers := hotnet.layers - 2;
			neurons := hotnet.neurons;
			inputs := hotnet.input;
			outputs := hotnet.output;
			Dialog.UpdateInt(inputs);
			Dialog.UpdateInt(outputs);
			Dialog.UpdateInt(layers);
			Dialog.UpdateInt(neurons);
			nnready := TRUE;
		ELSE
			Log.String("Version error!"); Log.Ln
		END
	END Internalize;
	
	PROCEDURE Savew*;
	BEGIN
		hotnet.Save
	END Savew;

	PROCEDURE Loadw*;
		VAR loc: Files.Locator; name: Files.Name; file: Files.File;
			rd: Stores.Reader; store: Stores.Store;
	BEGIN
		Dialog.GetIntSpec('mlp', loc, name);
		IF loc # NIL THEN
			file := Files.dir.Old(loc, name, Files.shared);
			rd.ConnectTo(file);
			rd.ReadStore(store);
			WITH store: NetModel.NeuroNet DO
				hotnet := store;
				layers := hotnet.layers - 2;
				neurons := hotnet.neurons;
				inputs := hotnet.input;
				outputs := hotnet.output;
				Dialog.UpdateInt(inputs);
				Dialog.UpdateInt(outputs);
				Dialog.UpdateInt(layers);
				Dialog.UpdateInt(neurons);
				nnready := TRUE;
				IF view # NIL THEN
					Views.Update(view, Views.keepFrames)
				END;
			ELSE
				Log.String("Version error!"); Log.Ln
			END;
			rd.ConnectTo(NIL)
		END
	END Loadw;

	PROCEDURE Kick*;
	BEGIN
		isKick := TRUE
	END Kick;

	PROCEDURE Stop*;
	BEGIN
		Services.RemoveAction(netteacher);
		nnready := TRUE;
		statstop := TRUE;
	END Stop;

	PROCEDURE LogReady;
	VAR mess: ARRAY 256 OF CHAR;
	BEGIN
		Log.Ln;
		Dialog.MapString("#Net:Network ready", mess);
		Log.String(mess + ": "); Log.Int(layers); Log.Int(neurons); Log.Ln;
		Dialog.MapString("#Net:Error:", mess);
		Log.String(mess + " "); Log.RealForm(aerror, 16, 0, 0, " "); Log.Ln;
		Dialog.MapString("#Net:Time:", mess);
		Log.String(mess + " "); Log.RealForm(time, 16, 0, 0, " "); Log.Ln;
		Dialog.MapString("#Net:Cycles:", mess);
		Log.String(mess); Log.Int(counter); Log.Ln;
		Dialog.MapString("#Net:Attempts:", mess);
		Log.String(mess); Log.Int(try); Log.Ln;
		
		IF view # NIL THEN
			Views.Update(view, Views.keepFrames)
		END;
				
	END LogReady;


	PROCEDURE (teacher: Teacher) Do;
		VAR i, cur, res: INTEGER;
		xa, ya: POINTER TO ARRAY OF REAL;
		error, margin: REAL;
	BEGIN
		NEW(xa, LEN(input, 0));
		NEW(ya, LEN(input, 0));
		
		REPEAT
			IF isKick THEN
				hotnet.Kick(kickamp); isKick := FALSE
			END;
			IF mix THEN teacher.m := Mixer(LEN(input, 0)) END;
			
			error := 0;
			FOR i := 0 TO LEN(input, 0) - 1 DO
				hotnet.Live(input[teacher.m[i]]);
				hotnet.Back(output[teacher.m[i]]);
				hotnet.Learn(speed);
				error := error + hotnet.CountError();
				IF teacher.single THEN
					xa[i] := output[teacher.m[i], 0];
					ya[i] := hotnet.out[hotnet.layers - 1, 0]
				END;
			END;
			IF temp.pos < LEN(temp.line) THEN
				temp.line[temp.pos] := error
			END;
			INC(temp.pos);
			
			INC(counter)
		UNTIL (error < errormax) OR (counter MOD responce = 0) OR nnready;
				
		aerror := error;
		Dialog.UpdateReal(aerror);
		IF view # NIL THEN
			Views.Update(view, Views.keepFrames)
		END;
		
		IF temp.pos > 2 THEN
			a.Clear;
			res := a.Plot1(temp.line, "");
			a.xmin := 0;
			a.xmax := temp.pos - 1;
			a.ymin := 0;
			a.ymax := Max(temp.line);
			Views.Update(a, Views.keepFrames)
		END;
		IF b # NIL THEN
			b.Clear;
			b.xmax := Max(xa);
			b.xmin := Min(xa); (* can be optimized *)
			margin := (b.xmax - b.xmin) / 10;
			b.xmax := b.xmax + margin;
			b.xmin := b.xmin - margin;
			b.ymin := b.xmin;
			b.ymax := b.xmax;
			(* can also plot trend line *)
			cur := b.Plot(xa, ya, "");
			b.SetStyle(cur, '.', 3, Ports.red);
			Views.Update(b, Views.keepFrames);
		END;
			
		time := (Kernel.Time() - time1) / 1000;
		Dialog.UpdateReal(time);
		Dialog.UpdateInt(counter);
		
		IF ABS(error) < errormax THEN
			nnready := TRUE;
			LogReady;
			Services.RemoveAction(teacher)
		ELSIF ~ nnready THEN
			IF ~ keep & timeoutReset & (time > timeout) THEN
				hotnet := NetModel.New(layers, neurons, inputs, outputs, wamp);
				time1 := Kernel.Time();
				INC(try); Dialog.UpdateInt(try);
				counter := 0;
			END;
			Services.DoLater(teacher, Services.now)
		END
	END Do;

	PROCEDURE Load1*;
	BEGIN
		tmp := NetService.Readmatrix(inputFile);
		IF tmp # NIL THEN
			input := tmp;
			inputs := LEN(input, 1);
			Dialog.UpdateInt(inputs);
			Dialog.UpdateString(inputFile)
		END
	END Load1;

	PROCEDURE Load2*;
	BEGIN
		tmp := NetService.Readmatrix(outputFile);
		IF tmp # NIL THEN
			output := tmp;
			outputs := LEN(output, 1);
			Dialog.UpdateInt(outputs);
			Dialog.UpdateString(outputFile)
		END
	END Load2;

	PROCEDURE ResetANN*;
	BEGIN
		hotnet := NetModel.New(layers, neurons, LEN(input, 1), LEN(output, 1), wamp);
	END ResetANN;

	PROCEDURE Teach*;
		VAR i, j, rep: INTEGER; error: REAL;
		result: POINTER TO ARRAY OF ARRAY OF REAL;
		m: POINTER TO ARRAY OF INTEGER;
		tmp, mess: ARRAY 256 OF CHAR; 
	BEGIN 
	
		(* Если все открылось нормально то обучаем сеть *)
		IF (input # NIL) & (output # NIL) THEN
			IF LEN(input, 0) # LEN(output, 0) THEN
				Dialog.MapString("#Net:Input and output lines does not match!", mess);
				Log.String(mess); Log.Ln;
			ELSE
				nnready := FALSE;
				(* Проверка параметров нейросети *)
				IF neurons < LEN(input, 1) THEN
					IF neurons < LEN(output, 1) THEN
						neurons := LEN(output, 1)
					ELSE
						neurons := LEN(input, 1)
					END;
					Dialog.MapString("#Net:MLP width changed to:", mess);
					Log.String(mess + " "); Log.Int(neurons); Log.Ln;
					Dialog.UpdateInt(neurons)
				END;

				IF teachtest THEN
					
					backupnet := hotnet;
					
					(* leave-one-out test *)
					NEW(result, LEN(output, 0) * repeate, LEN(output, 1));
					
					counter := 0;
					time1 := Kernel.Time();
					time := 0;
					Dialog.UpdateReal(time);
					
					IF ~ mix THEN
						NEW(m, LEN(input));
						FOR i := 0 TO LEN(input) - 1 DO m[i] := i END
					END;
					
					FOR rep := 0 TO repeate - 1 DO
					
							FOR j := 0 TO LEN(input, 0) - 1 DO
								IF mix THEN m := Mixer(LEN(input, 0)) END;
								nnready := FALSE;
								Log.String("Teach without line #"); Log.Int(j + 1); Log.Ln;
								try := 1;
								Dialog.UpdateInt(try);
								
								hotnet := NetModel.New(layers, neurons, inputs, outputs, wamp);
								error := errormax + 1;
								counter := 0;
								
								WHILE (error > errormax) & (~nnready) DO
									IF isKick THEN
										hotnet.Kick(kickamp);
										isKick := FALSE;
									END;
									
									error := 0;
									FOR i := 0 TO LEN(input, 0) - 1 DO
										IF m[i] # j THEN
											hotnet.Live(input[m[i]]);
											hotnet.Back(output[m[i]]);
											hotnet.Learn(speed);
											error := error + hotnet.CountError()
										END
									END;
									
									IF counter MOD responce = 0 THEN
										time := (Kernel.Time() - time1) / 1000;
										Dialog.UpdateReal(time);
										Dialog.UpdateInt(counter);
										IF timeoutReset & (time > timeout) THEN
											hotnet := NetModel.New(layers, neurons, inputs, outputs, wamp);
											time1 := Kernel.Time();
											INC(try); Dialog.UpdateInt(try);
											counter := 0;
										END;
										IF isKick THEN
											hotnet.Kick(kickamp);
											isKick := FALSE;
										END;
										aerror := ABS(error);
										Dialog.UpdateReal(aerror);
										ProcessMessages
									END;
												
									INC(counter)
								END;

								hotnet.Live(input[j]);
								FOR i := 0 TO hotnet.output - 1 DO
									result[rep*LEN(output, 0) + j, i] := hotnet.out[hotnet.layers - 1, i]
								END;
							END;
							nnready := TRUE
					END;
					hotnet := backupnet;
					
					NetService.Savematrix(result)
				
				ELSIF showdinamics THEN
					IF ~ keep THEN
						hotnet := NetModel.New(layers, neurons, inputs, outputs, wamp);
					END;
					
					Dialog.MapString("#Net:Learning dynamics", tmp);
					IF GetWindow(tmp$ + " 1") = NIL THEN
						a := DiaPlot.dir.New();
						a.grid := TRUE;
						a.title := tmp$ + " 1";
						Dialog.MapString("#Net:Time, ms", a.xtitle);
						Dialog.MapString("#Net:Error", a.ytitle);
						a.Show
					END;
					
					NEW(netteacher);
					netteacher.single := outputs = 1;
					IF netteacher.single THEN
						IF GetWindow(tmp$ + " 2") = NIL THEN
							Dialog.MapString("#Net:Learning dynamics", tmp);
							b := DiaPlot.dir.New();
							b.grid := TRUE;
							b.title := tmp$ + " 2";
							Dialog.MapString("#Net:Expected", b.xtitle);
							Dialog.MapString("#Net:Prediction", b.ytitle);
							b.Show
						END
					END;
					
					FOR i := 0 TO LEN(temp.line) - 1 DO temp.line[i] := 0 END;
					temp.pos := 0;
					
					counter := 0;
					time1 := Kernel.Time();
					time := 0;
					Dialog.UpdateReal(time);
					try := 1;
					Dialog.UpdateInt(try);
					IF ~ mix THEN
						NEW(netteacher.m, LEN(input));
						FOR i := 0 TO LEN(input) - 1 DO netteacher.m[i] := i END
					END;
					error := MAX(REAL);
					
					Services.DoLater(netteacher, Services.now)

				ELSE					
					
					(* FAST *)
					IF ~ keep THEN
						hotnet := NetModel.New(layers, neurons, inputs, outputs, wamp)
					END;
					counter := 0;
					time1 := Kernel.Time();
					time := 0;
					Dialog.UpdateReal(time);
					try := 1;
					Dialog.UpdateInt(try);
					IF ~ mix THEN
						NEW(m, LEN(input));
						FOR i := 0 TO LEN(input) - 1 DO m[i] := i END
					END;

					error := MAX(REAL);
					
					REPEAT
						IF mix THEN m := Mixer(LEN(input, 0)) END;
						(* Пока суммарная ошибка по всем выходам не меньше заданой *)
						IF counter MOD responce = 0 THEN
							time := (Kernel.Time() - time1) / 1000;
							Dialog.UpdateReal(time);
							Dialog.UpdateInt(counter);
							IF ~ keep & timeoutReset & (time > timeout) THEN
								hotnet := NetModel.New(layers, neurons, inputs, outputs, wamp);
								time1 := Kernel.Time();
								INC(try); Dialog.UpdateInt(try);
								counter := 0;
							END;
							IF isKick THEN
								hotnet.Kick(kickamp);
								isKick := FALSE;
							END;
							aerror := ABS(error);
							Dialog.UpdateReal(aerror);
							ProcessMessages
						END;
						
						error := 0;
						FOR i := 0 TO LEN(input, 0) - 1 DO
							hotnet.Live(input[m[i]]);
							hotnet.Back(output[m[i]]);
							hotnet.Learn(speed);
							error := error + hotnet.CountError()
						END;
						
						INC(counter)
					UNTIL (error < errormax) OR nnready;
					
					time := (Kernel.Time() - time1) / 1000;
					Dialog.UpdateReal(time);
					Dialog.UpdateInt(counter);
					aerror := ABS(error);
					Dialog.UpdateReal(aerror);
					
					LogReady;
					
					IF view # NIL THEN Views.Update(view, TRUE) END;
					nnready := TRUE
					
				END
				
			END
		END
	END Teach;

	PROCEDURE TeachTest*;
	BEGIN
		teachtest := TRUE;
		Teach;
		teachtest := FALSE;
	END TeachTest;

	PROCEDURE Aver(a: ARRAY OF REAL): REAL;
	VAR i: INTEGER; res: REAL;
	BEGIN
		res := 0;
		FOR i:= 0 TO LEN(a) - 1 DO
			res := res + a[i]
		END;
		RETURN res / LEN(a)
	END Aver;

	PROCEDURE StdDev(a: ARRAY OF REAL): REAL;
	VAR i: INTEGER; aver, res: REAL;
	BEGIN
		aver := Aver(a);
		(* Log.Ln; Log.Char(">"); Log.Real(aver); Log.Ln; *)
		res := 0;
		FOR i:= 0 TO LEN(a) - 1 DO
			res := res + Math.IntPower(a[i] - aver, 2)
		END;
		RETURN Math.Sqrt(res / ((LEN(a) - 1)*LEN(a)))
	END StdDev;

	PROCEDURE Stat*;
	VAR
		cyclesA, timeA, attemptsA: POINTER TO ARRAY OF REAL;
		lmem: INTEGER; 
	BEGIN
		statstop := FALSE;
		lmem := layers;
		layers := l1;
		Dialog.UpdateInt(layers);
		WHILE (layers <= l2) & ~ statstop DO
			
			NEW(cyclesA, times);
			NEW(timeA, times);
			NEW(attemptsA, times);
			statStep := 1;
			WHILE (statStep <= times) & ~ statstop DO
				Dialog.UpdateInt(statStep);
				Teach();
				cyclesA[statStep-1] := counter;
				timeA[statStep-1] := time;
				attemptsA[statStep-1] := try;
				INC(statStep)
			END;
			
			Log.Int(layers); Log.Tab;
			Log.Int(neurons); Log.Ln;
			
			Log.RealForm(Aver(cyclesA), 16, 0, -3, ' '); Log.Tab;
			Log.RealForm(Aver(timeA), 16, 0, -3, ' '); Log.Tab;
			Log.RealForm(Aver(attemptsA), 16, 0, -3, ' '); Log.Ln;
			
			IF times > 1 THEN
				Log.RealForm(StdDev(cyclesA), 16, 0, -3, ' '); Log.Tab;
				Log.RealForm(StdDev(timeA), 16, 0, -3, ' '); Log.Tab;
				Log.RealForm(StdDev(attemptsA), 16, 0, -3, ' '); Log.Ln;
			END;
			
			INC(layers);
			Dialog.UpdateInt(layers);
		END;
		layers := lmem;
		Dialog.UpdateInt(layers);
	END Stat;

	PROCEDURE Predict*;
		VAR input: POINTER TO ARRAY OF ARRAY OF REAL;
			name, mess: ARRAY 256 OF CHAR;
			i, j, k: INTEGER;
	BEGIN
		tmp := NetService.Readmatrix(name);
		IF tmp # NIL THEN
			IF LEN(tmp, 1) # hotnet.input THEN
				Dialog.MapString("#Net:Data does not fit inputs", mess);
				Log.String(mess); Log.Ln
			ELSE
				Dialog.MapString("#Net:Prediction for:", mess);
				Log.String(mess + " " + name); Log.Ln;
				FOR i := 0 TO LEN(tmp) - 1 DO
					hotnet.Live(tmp[i]);
					k := LEN(hotnet.out, 0) - 1;
					FOR j := 0 TO hotnet.output - 1 DO
						Log.RealForm(hotnet.out[k, j], 16, 0, -6, ' '); Log.Tab
					END;
					Log.Ln
				END;
				Log.Ln
			END
		END
	END Predict;

	PROCEDURE Demo*;
		VAR input: ARRAY 2 OF REAL;
		i, k, len: INTEGER;
		map: POINTER TO ARRAY OF ARRAY OF REAL;
	BEGIN
		IF hotnet # NIL THEN
			NEW(map, nnn, nnn);
			FOR i := 0 TO nnn-1 DO
				FOR k := 0 TO nnn-1 DO
					input[0] := x1 + i*(x2 - x1)/nnn;
					input[1] := y1 + k*(y2 - y1)/nnn;
					hotnet.Live(input);
					len := LEN(hotnet.out);
					map[i, k] := hotnet.out[len-1, 0]
				END
			END;
			NetPlotMap.Plot(map, "in 1", "in 2", x1, x2, y1, y2)
		END;
	END Demo;

	PROCEDURE Notifier* (op, from, to: INTEGER);
	BEGIN
		IF op = Dialog.changed THEN
			speed := tspeed / 50;
			Dialog.UpdateReal(speed)
		END
	END Notifier;

	PROCEDURE Notifier2* (op, from, to: INTEGER);
	BEGIN
		IF op = Dialog.changed THEN
			IF speed < 0 THEN speed := 0 END;
			tspeed :=  SHORT(ENTIER(speed * 50));
			Dialog.UpdateInt(tspeed)
		END
	END Notifier2;

	PROCEDURE HiddenLayersNotifier* (op, from, to: INTEGER);
	BEGIN
		IF op = Dialog.changed THEN
			IF layers < 1 THEN layers := 1; Dialog.UpdateInt(layers) END
		END
	END HiddenLayersNotifier;

	PROCEDURE GuardPredict* (VAR par: Dialog.Par);
	BEGIN
		IF hotnet # NIL THEN
			par.disabled := FALSE
		ELSE
			par.disabled := TRUE
		END
	END GuardPredict;

	PROCEDURE GuardStop* (VAR par: Dialog.Par);
	BEGIN
		IF hotnet = NIL THEN
			par.disabled := TRUE
		ELSE
			par.disabled := nnready
		END
	END GuardStop;

	PROCEDURE GuardStart* (VAR par: Dialog.Par);
	BEGIN
		IF hotnet = NIL THEN
			par.disabled := FALSE
		ELSE
			par.disabled := ~ nnready
		END
	END GuardStart;
	
	PROCEDURE GuardKeep* (VAR par: Dialog.Par);
	BEGIN
		par.disabled := keep
	END GuardKeep;

	PROCEDURE GuardStart2* (VAR par: Dialog.Par);
	BEGIN
		par.disabled := FALSE;
		IF keep THEN 
			par.readOnly := TRUE
		ELSE
			IF hotnet = NIL THEN
				par.readOnly := FALSE
			ELSE
				par.readOnly := ~ nnready
			END
		END
	END GuardStart2;

	PROCEDURE GuardTeach* (VAR par: Dialog.Par);
	BEGIN
		IF (input = NIL) OR (output = NIL) THEN
			par.disabled := TRUE
		ELSIF hotnet = NIL THEN
			par.disabled := FALSE
		ELSE
			par.disabled := ~ nnready
		END
	END GuardTeach;

	PROCEDURE GuardDemo* (VAR par: Dialog.Par);
	BEGIN
		IF (hotnet # NIL) & (hotnet.input = 2) &  (hotnet.output = 1) THEN
			par.disabled := FALSE
		ELSE par.disabled := TRUE END
	END GuardDemo;

	PROCEDURE Init*;
	BEGIN
		StdCmds.OpenAuxDialog('Net/Rsrc/Main.odc', '#Net:Control panel');
	END Init;

	PROCEDURE Init2*;
	BEGIN
		StdCmds.OpenAuxDialog('Net/Rsrc/Extra.odc', '#Net:Extra panel');
	END Init2;

	PROCEDURE Docu*;
	BEGIN
		IF Dialog.language = 'ru' THEN
			StdCmds.OpenAux('Net/Docu/ru/Manual.odc', '#Net:Manual');
		ELSE
			StdCmds.OpenAux('Net/Docu/Manual.odc', '#Net:Manual');
		END
	END Docu;

BEGIN
	keep := FALSE;
	try := 1;
	layers := 1;
	neurons := 4;
	inputs := 0;
	outputs := 0;
	teachtest := FALSE;
	repeate := 1;
	showdinamics := FALSE;
	timeoutReset := TRUE;
	speed := 1;
	timeout := 100;
	tspeed := 50;
	errormax := 0.1;
	nnready := FALSE;
	statstop := TRUE;
	nnn := 50;
	mix := TRUE;
	x1 := 0;
	x2 := 1;
	y1 := 0;
	counter := 0;
	y2 := 1;
	times := 1;
	statStep := 0;
	time := 0;
	Dialog.MapString("#Net:choose file", inputFile);
	Dialog.MapString("#Net:choose file", outputFile);
	rand := NetRandom.New();
	l1 := 1;
	l2 := 1;
	w1 := 3;
	w2 := 3;
	wamp := 2;  (* initial weights from -1 to 1 *)
	kickamp := 0.2;	(* initial weights from -0.1 to 0.1 *)
	
END NetMain.

DevDebug.UnloadThis NetMain
