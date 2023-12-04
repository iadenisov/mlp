MODULE NetModel;
	(**
	MLP core model.
	Author: Ivan Denisov
	Refs: Wasserman, Philip D., Neural Computing: Theory and practice,
		Van Nostrand Reinhold, Inc., 1989.
	**)

	IMPORT Files, Stores, Dialog, Math, NetRandom;

	TYPE
		NeuroNet* = POINTER TO RECORD (Stores.Store)
			w-: POINTER TO ARRAY OF ARRAY OF ARRAY OF REAL;
			input-, output-, layers-, neurons-: INTEGER;
			out-, delta: POINTER TO ARRAY OF ARRAY OF REAL;
			error:  POINTER TO ARRAY OF REAL;
			gen: NetRandom.RandomGenerator;
		END;

	PROCEDURE F (in: REAL): REAL;
	BEGIN
		RETURN 1 / (1 + Math.Exp(- in))
	END F;

	PROCEDURE (nn: NeuroNet) Externalize- (VAR wr: Stores.Writer);
		VAR i, j, k: INTEGER;
	BEGIN
		wr.WriteInt(nn.input);
		wr.WriteInt(nn.output);
		wr.WriteInt(nn.layers);
		wr.WriteInt(nn.neurons);
		FOR k := 0 TO nn.layers - 1 DO
			FOR i := 0 TO nn.neurons - 1 DO
				FOR j := 0 TO nn.neurons - 1 DO
					wr.WriteReal(nn.w[k, i, j])
				END
			END
		END
	END Externalize;

	PROCEDURE (nn: NeuroNet) Internalize- (VAR rd: Stores.Reader);
		VAR i, j, k: INTEGER;
	BEGIN
		rd.ReadInt(nn.input);
		rd.ReadInt(nn.output);
		rd.ReadInt(nn.layers);
		rd.ReadInt(nn.neurons);
		NEW(nn.w, nn.layers, nn.neurons, nn.neurons);
		NEW(nn.out, nn.layers, nn.neurons);
		NEW(nn.delta, nn.layers, nn.neurons);
		NEW(nn.error, nn.output);
		FOR k := 0 TO nn.layers - 1 DO
			FOR i := 0 TO nn.neurons - 1 DO
				FOR j := 0 TO nn.neurons - 1 DO
					rd.ReadReal(nn.w[k, i, j])
				END
			END
		END
	END Internalize;

	PROCEDURE (nn: NeuroNet) Save*, NEW;
	VAR loc: Files.Locator; name: Files.Name; file: Files.File; wr: Stores.Writer; res: INTEGER;
	BEGIN
		loc := NIL; name := 'new';
		Dialog.GetExtSpec(name, 'mlp', loc, name);
		IF loc # NIL THEN
			file := Files.dir.New(loc, Files.dontAsk);
			wr.ConnectTo(file);
			wr.WriteStore(nn);
			file.Register(name, '', Files.dontAsk, res);
			wr.ConnectTo(NIL)
		END
	END Save;
	
	PROCEDURE (nn: NeuroNet) Kick* (kickamp: REAL), NEW;
		VAR i, j, k: INTEGER;
	BEGIN
		FOR k := 0 TO nn.layers - 1 DO
			FOR j := 0 TO nn.neurons - 1 DO
				FOR i := 0 TO nn.neurons - 1 DO
					nn.w[k, j, i] := nn.w[k, j, i] + (nn.gen.Uniform() - 0.5) * kickamp
				END
			END
		END
	END Kick;

	PROCEDURE (nn: NeuroNet) Live* (VAR in: ARRAY OF REAL), NEW;
		VAR i, j, k: INTEGER; sum: REAL;
	BEGIN
		(* Copy inputs to the first layer *)
		FOR i := 0 TO nn.input - 1 DO
			nn.out[0, i] := in[i]
		END;
		(* Propagate from the first layer to the second *)
		FOR j := 0 TO nn.neurons - 1 DO
			sum := 0;
			FOR i := 0 TO nn.input - 1 DO
				sum := sum + nn.out[0, i] * nn.w[0, i, j]
			END;
			nn.out[1, j] := F(sum)
		END;
		IF nn.layers > 3 THEN
			(* Propagate from the second layer to one before the last *)
			FOR k := 1 TO nn.layers - 3 DO
				FOR j := 0 TO nn.neurons - 1 DO
					sum := 0;
					FOR i := 0 TO nn.neurons - 1 DO
						sum := sum + nn.out[k, i] * nn.w[k, i, j]
					END;
					nn.out[k + 1, j] := F(sum)
				END
			END
		END;
		(* Prepaing outputs in the last layer *)
		FOR j := 0 TO nn.output - 1 DO
			sum := 0;
			FOR i := 0 TO nn.neurons - 1 DO
				sum := sum + nn.out[nn.layers - 2, i] * nn.w[nn.layers - 2, i, j]
			END;
			nn.out[nn.layers - 1, j] := F(sum)
		END;
	END Live;

	PROCEDURE (nn: NeuroNet) Back* (VAR out: ARRAY OF REAL), NEW;
		VAR i, j, k: INTEGER; sum: REAL;
	BEGIN
		k := nn.layers - 1;
		FOR i := 0 TO nn.output - 1 DO
			nn.error[i] := out[i] - nn.out[k, i]; (* put the error here *)
			nn.delta[k, i] := nn.out[k, i] * (1 - nn.out[k, i]) * nn.error[i]
		END;
		FOR k := k - 1 TO 0 BY - 1 DO
			FOR j := 0 TO nn.neurons - 1 DO
				sum := 0;
				FOR i := 0 TO nn.neurons - 1 DO
					sum := sum + nn.delta[k + 1, i] * nn.w[k, j, i]
				END;
				nn.delta[k, j] := nn.out[k, j] * (1 - nn.out[k, j]) * sum
			END
		END
	END Back;

	PROCEDURE (nn: NeuroNet) Learn* (speed: REAL), NEW;
		VAR i, j, k: INTEGER;
	BEGIN
		FOR k := nn.layers - 1 TO 1 BY - 1 DO
			FOR i := 0 TO nn.neurons - 1 DO
				FOR j := 0 TO nn.neurons - 1 DO
					nn.w[k-1, i, j] := nn.w[k-1, i, j] + speed * nn.out[k-1, i] * nn.delta[k, j]
				END
			END
		END
	END Learn;

	PROCEDURE (nn: NeuroNet) CountError*(): REAL, NEW;
		VAR i: INTEGER; s: REAL;
	BEGIN
		s := 0;
		FOR i := 0 TO nn.output - 1 DO
			s := s + ABS(nn.error[i])
		END;
		RETURN s
	END CountError;

	PROCEDURE New* (layers, neurons, input, output: INTEGER; wamp: REAL): NeuroNet;
		VAR i, j, k: INTEGER; nn: NeuroNet;
	BEGIN
		NEW(nn);
		nn.input := input;
		nn.output := output;
		nn.layers := layers + 2;
		nn.neurons := neurons;
		nn.gen := NetRandom.New();
		NEW(nn.w, nn.layers, nn.neurons, nn.neurons);
		NEW(nn.out, nn.layers, nn.neurons);
		NEW(nn.delta, nn.layers, nn.neurons);
		NEW(nn.error, nn.output);
		FOR k := 0 TO nn.layers - 1 DO
			FOR j := 0 TO nn.neurons - 1 DO
				FOR i := 0 TO nn.neurons - 1 DO
					nn.w[k, j, i] := (nn.gen.Uniform() - 0.5) * wamp
				END
			END
		END;
		RETURN nn
	END New;

END NetModel.