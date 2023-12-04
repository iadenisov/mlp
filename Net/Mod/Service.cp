MODULE NetService;

	

	IMPORT Converters, DiaPlot, Dialog, Files, Ports, Log, Strings, TextMappers, TextModels, TextViews, Views;

	PROCEDURE Readmatrix* (VAR path: ARRAY OF CHAR): POINTER TO ARRAY OF ARRAY OF REAL;
		VAR s: TextMappers.Scanner;
			i, j, rows, cols: INTEGER;
			loc: Files.Locator;
			name: Files.Name;
			conv: Converters.Converter;
			v: Views.View;
			t: TextModels.Model;
			data: POINTER TO ARRAY OF ARRAY OF REAL;
			coma, semicolumn: BOOLEAN;
	BEGIN
		Dialog.GetIntSpec('csv', loc, name);
		IF loc # NIL THEN
			path := name$;
			conv := Converters.list;
			WHILE (conv # NIL) & (conv.fileType # 'txt') DO conv := conv.next END;
			v := Views.Old(Views.dontAsk, loc, name, conv);
			t := v(TextViews.View).ThisModel();
			s.ConnectTo(t);
			
			(* Check format *)
			s.SetOpts({1});
			s.Scan;
			coma := FALSE; semicolumn := FALSE;
			WHILE (s.type # TextMappers.eot) & (~ coma OR ~ semicolumn) DO
				IF ~ coma & (s.type = TextMappers.char) & (s.char = ",") THEN
					coma := TRUE
				END;
				IF ~ semicolumn & (s.type = TextMappers.char) & (s.char = ";") THEN
					semicolumn := TRUE
				END;
				s.Scan
			END;
			IF coma & semicolumn THEN
				Log.String("CSV format error!"); Log.Ln;
				Log.String("Replace commas by dots in delimiters"); Log.Ln;
				RETURN NIL
			END;
			
			(* Define the size of array *)
			s.SetPos(0);
			s.Scan;
			cols := 0;
			(* Define width and then check *)
			WHILE (s.type # TextMappers.line) & (s.type # TextMappers.eot) DO
				IF (s.type = TextMappers.int) OR (s.type = TextMappers.real) THEN
					INC(cols);
				END;
				s.Scan
			END;
			rows := 1;
			IF s.type # TextMappers.eot THEN
				i := 0;
				REPEAT
					s.Scan;
					IF (s.type = TextMappers.int) OR (s.type = TextMappers.real) THEN
						INC(i)
					END;
					IF (s.type = TextMappers.line) OR (s.type = TextMappers.eot) THEN
						IF (i = 0) & (s.type # TextMappers.eot) THEN
							Log.String('Epmty string!'); Log.Ln
						ELSIF i = cols THEN
							i := 0;
							INC(rows)
						ELSIF (s.type # TextMappers.eot) THEN
							Log.String('Amount of elements in the line do not match!'); Log.Ln
						END
					END
				UNTIL s.type = TextMappers.eot
			END;
			s.SetOpts({});
			(* Create an array *)
			NEW(data, rows, cols);
			(* Записываем данные в массив *)
			s.SetPos(0);
			i := 0;
			WHILE (i < rows) & (s.type # TextMappers.eot) DO
				j := 0;
				WHILE (j < cols) & (s.type # TextMappers.line) & (s.type # TextMappers.eot) DO
					s.Scan;
					IF s.type = TextMappers.int THEN
						data[i, j] := s.int;
						INC(j)
					ELSIF s.type = TextMappers.real THEN
						data[i, j] := s.real;
						INC(j)
					END
				END;
				INC(i)
			END;
			s.ConnectTo(NIL);
			v := NIL;
			RETURN data
		ELSE
			RETURN NIL
		END
	END Readmatrix;

	PROCEDURE Savematrix* (w: ARRAY OF ARRAY OF REAL);
		VAR loc: Files.Locator; name: Files.Name; conv: Converters.Converter;
			v: Views.View; t: TextModels.Model; f: TextMappers.Formatter;
			res, i, j: INTEGER;
	BEGIN
		loc := NIL; name := "";
		conv := Converters.list;
		WHILE (conv # NIL) & (conv.fileType # 'txt') DO conv := conv.next END;
		t := TextModels.dir.New();
		f.ConnectTo(t);
		FOR i := 0 TO LEN(w, 0) - 1 DO
			FOR j := 0 TO LEN(w, 1) - 1 DO
				f.WriteReal(w[i][j]); f.WriteTab
			END; f.WriteLn
		END;
		v := TextViews.dir.New(t);
		Views.Register(v, Views.ask, loc, name, conv, res);
	END Savematrix;

	PROCEDURE Logmatrix* (input: ARRAY OF ARRAY OF REAL; name: ARRAY OF CHAR);
		VAR i, j: INTEGER;
	BEGIN
		Log.String(name); Log.Ln;
		FOR i := 0 TO LEN(input, 0) - 1 DO
			FOR j := 0 TO LEN(input, 1) - 1 DO
				Log.RealForm(input[i][j], 3, 0, 0, 8FX); Log.String(" ")
			END; Log.Ln
		END; Log.Ln
	END Logmatrix;

	PROCEDURE Int (i: INTEGER): POINTER TO ARRAY OF CHAR;
		VAR m: POINTER TO ARRAY OF CHAR;
	BEGIN
		NEW(m, 64); Strings.IntToString(i, m); RETURN m
	END Int;

	PROCEDURE Plotmany*;
		VAR y: POINTER TO ARRAY OF ARRAY OF REAL; i, res: INTEGER;
			a: DiaPlot.Axes; x: POINTER TO ARRAY OF REAL;
			path: ARRAY 256 OF CHAR;
	BEGIN
		a := DiaPlot.dir.New();
		a.xtitle := "X";
		a.ytitle := "Y";
		a.grid := TRUE;
		y := Readmatrix(path);
		IF y # NIL THEN
			NEW(x, LEN(y, 1));
			FOR i := 0 TO LEN(y, 1) - 1 DO
				x[i] := i
			END;
			FOR i := 0 TO LEN(y, 0) - 1 DO
				res := a.Plot(x, y[i], "line #" + Int(i));
				a.SetStyle(res, '-', 1, Ports.black)
			END;
			a.Show
		END
	END Plotmany;


END NetService.