MODULE NetSliders; 	(*  Date :  26  April  2005    *)
	(*  Author :  Robert D Campbell  *)
	(*  Copyright :  Robert D Campbell  *)

	IMPORT Controllers, Controls, Dialog, Fonts, Meta, Ports, Properties, StdCFrames, Stores, Strings, Views;


	CONST
		minVersion = 0; maxVersion = 0;
		fill = Ports.fill;
		aMax = 100;
		demi = 2 * Ports.mm;
		knob = 2 * demi;
		maxLen = 1;

		leftArrow = 1CX; rightArrow = 1DX; upArrow = 1EX; downArrow = 1FX;

	TYPE
		Pair* = RECORD a*, b*, minGap*, abMax*: INTEGER END;
		Slider = POINTER TO RECORD (Controls.Control) colour: Ports.Color END;
		TwinSlider = POINTER TO RECORD (Controls.Control) END;
		CharField = POINTER TO RECORD (Controls.Control) END;

		PropOp = POINTER TO RECORD (Stores.Operation)
			c: Slider;
			newProp, oldProp: Properties.Property;
			do: BOOLEAN
		END;

	VAR
		pair*: Pair;
		arial8pt: Fonts.Font;


	PROCEDURE Constrain (VAR n: INTEGER; min, max: INTEGER);
	BEGIN
		ASSERT(min <= max, 20); n := MAX(min, MIN(n, max))
	END Constrain;

	(*  *****  *****  *****  *****  Slider control  *****  *****  *****  *****  *)

	PROCEDURE Paint (c: Slider; f: Views.Frame; txtFlg: BOOLEAN);
		CONST
			aCol = Ports.grey12;
		VAR
			w, h, a, aPos, bPos, x, len, twoDot: INTEGER;
			str: ARRAY 4 OF CHAR;
	BEGIN
		c.context.GetSize(w, h);
		IF ~c.disabled & c.item.Valid() THEN
			a := c.item.IntVal(); Constrain(a, 0, aMax); twoDot := 2 * f.dot;
			IF w > h THEN
				len := w - twoDot - knob;
				aPos := f.dot + SHORT(LONG(len) * a DIV aMax); bPos := aPos + knob;
				IF a > 0 THEN f.DrawRect(f.dot, f.dot, aPos, h - f.dot, fill, Ports.background) END;

				f.DrawRect(aPos, f.dot, bPos, h - f.dot, f.dot, Ports.black);
				f.DrawRect(aPos + f.dot, twoDot, bPos - f.dot, h - twoDot, fill, c.colour);
				IF a < aMax THEN f.DrawRect(bPos, f.dot, w - f.dot, h - f.dot, fill, aCol) END;
				IF txtFlg THEN
					IF 3 * a >= aMax THEN x := aPos DIV 2 ELSE x := (bPos + w) DIV 2 END;
					Strings.IntToString(a, str);
					(* f.DrawString(x - Ports.mm, h DIV 2 + Ports.mm, Ports.black, str, arial8pt) *)
				END
			ELSE
				len := h - twoDot - knob;
				aPos := f.dot + SHORT(LONG(len) * (aMax - a) DIV aMax);
				bPos := aPos + knob;
				IF a > 0 THEN f.DrawRect(f.dot, bPos, w - f.dot, h - f.dot, fill, Ports.background) END;
				f.DrawRect(f.dot, aPos, w - f.dot, bPos, f.dot, Ports.black);
				f.DrawRect(twoDot, aPos + f.dot, w - twoDot, bPos - f.dot, fill, c.colour);
				IF a < aMax THEN f.DrawRect(f.dot, f.dot, w - f.dot, aPos, fill, aCol) END
			END
		ELSE
			f.DrawRect(f.dot, f.dot, w - f.dot, h - f.dot, fill, Ports.grey50)
		END;
		f.DrawRect(0, 0, w, h, f.dot, Ports.defaultColor)
	END Paint;


	PROCEDURE (c: Slider) UpdateValue (a: INTEGER;
	VAR aOld: INTEGER; f: Views.Frame), NEW;
	BEGIN
		Constrain(a, 0, aMax);
		IF a # aOld THEN
			c.item.PutIntVal(a); aOld := a;
			Paint(c, f, TRUE);
			Controls.Notify(c, f, Dialog.changed, 1, 2)
		END
	END UpdateValue;


	PROCEDURE Track (c: Slider; f: Views.Frame; VAR msg: Controllers.TrackMsg);
		VAR
			aOld, x, y, w, h, half, a: INTEGER;
			m: SET;
			horiz, isDown: BOOLEAN;
	BEGIN
		aOld := c.item.IntVal();
		c.context.GetSize(w, h);
		horiz := w > h;
		IF horiz THEN DEC(w, 2 * f.dot + knob); half := w DIV 2 ELSE DEC(h, 2 * f.dot + knob); half := h DIV 2 END;
		Paint(c, f, TRUE);
		REPEAT
			f.Input(x, y, m, isDown);
			IF horiz THEN a := (aMax * (x - demi) + half) DIV w ELSE a := aMax - (aMax * (y - demi) + half) DIV h END;
			c.UpdateValue(a, aOld, f)
		UNTIL ~isDown;
		Paint(c, f, FALSE)
	END Track;


	PROCEDURE (c: Slider) CopyFromSimpleView2- (source: Controls.Control);
	BEGIN
		c.colour := source(Slider).colour
	END CopyFromSimpleView2;


	PROCEDURE (c: Slider) Internalize2 (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version);
		IF ~rd.cancelled THEN rd.ReadInt(c.colour) END
	END Internalize2;


	PROCEDURE (c: Slider) Externalize2 (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion); wr.WriteInt(c.colour)
	END Externalize2;


	PROCEDURE (c: Slider) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		Paint(c, f, FALSE)
	END Restore;


	PROCEDURE (c: Slider) HandleCtrlMsg2 (f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
		VAR a, aOld, w, h: INTEGER;
	BEGIN
		IF ~c.disabled & ~c.readOnly & c.item.Valid() THEN
			WITH msg: Controllers.PollCursorMsg DO msg.cursor := Ports.graphicsCursor
			| msg: Controllers.TrackMsg DO Track(c, f, msg)
			| msg: Controllers.EditMsg DO
				IF msg.op = Controllers.pasteChar THEN
					a := c.item.IntVal(); aOld := a;
					c.context.GetSize(w, h);
					IF w > h THEN
						IF msg.char = leftArrow THEN DEC(a) ELSIF msg.char = rightArrow THEN INC(a) END
					ELSE
						IF msg.char = upArrow THEN INC(a) ELSIF msg.char = downArrow THEN DEC(a) END
					END;
					c.UpdateValue(a, aOld, f)
				END
			| msg: Controllers.MarkMsg DO
				IF msg.focus & ~msg.show THEN Paint(c, f, FALSE) END
			ELSE
			END
		END
	END HandleCtrlMsg2;


	PROCEDURE CollectProperties (c: Slider; VAR prop: Properties.Property);
		VAR stdProp: Properties.StdProp;
	BEGIN
		NEW(stdProp);
		stdProp.color.val := c.colour;
		stdProp.valid := {Properties.color};
		stdProp.known := {Properties.color};
		Properties.Insert(prop, stdProp)
	END CollectProperties;


	PROCEDURE SetProperties (c: Slider; prop: Properties.Property);
	BEGIN
		WHILE prop # NIL DO
			WITH prop: Properties.StdProp DO
				IF Properties.color IN prop.valid THEN c.colour := prop.color.val END
			ELSE
			END;
			prop := prop.next
		END
	END SetProperties;


	PROCEDURE (op: PropOp) Do;
	BEGIN
		IF op.do THEN SetProperties(op.c, op.newProp) ELSE SetProperties(op.c, op.oldProp) END;
		Views.Update(op.c, Views.keepFrames);
		op.do := ~op.do
	END Do;


	PROCEDURE NewPropOp (c: Slider; newProp, oldProp: Properties.Property): PropOp;
		VAR op: PropOp;
	BEGIN
		NEW(op); op.c := c;
		op.newProp := newProp; op.oldProp := oldProp; op.do := TRUE; RETURN op
	END NewPropOp;


	PROCEDURE (c: Slider) HandlePropMsg2 (VAR msg: Properties.Message);
		VAR p: Properties.Property;
	BEGIN
		WITH msg: Properties.FocusPref DO
			IF ~c.disabled & ~c.readOnly THEN msg.setFocus := TRUE END
		| msg: Controls.PropPref DO EXCL(msg.valid, Controls.label)
		| msg: Properties.SizePref DO
			IF msg.w > msg.h THEN msg.w := MAX(25 * Ports.mm, msg.w)
			ELSE msg.h := MAX(25 * Ports.mm, msg.h) END;
			msg.w := MAX(4 * Ports.mm, msg.w);
			msg.h := MAX(4 * Ports.mm, msg.h)
		| msg: Properties.PollMsg DO CollectProperties(c, msg.prop)
		| msg: Properties.SetMsg DO
			CollectProperties(c, p);
			Views.Do(c, 'Slider colour', NewPropOp(c, msg.prop, p))
		ELSE
		END
	END HandlePropMsg2;


	PROCEDURE (c: Slider) CheckLink (VAR ok: BOOLEAN);
	BEGIN
		ok := c.item.typ = Meta.intTyp
	END CheckLink;


	PROCEDURE (c: Slider) Update (f: Views.Frame; op, from, to: INTEGER);
	BEGIN
		Paint(c, f, FALSE)
	END Update;


	PROCEDURE NewSlider* (p: Controls.Prop): Controls.Control;
		VAR c: Slider;
	BEGIN
		NEW(c); c.colour := Ports.grey50; Controls.OpenLink(c, p); RETURN c
	END NewSlider;


	PROCEDURE DepositSlider*;
		VAR p: Controls.Prop;
	BEGIN
		NEW(p); Views.Deposit(NewSlider(p))
	END DepositSlider;

	(*  *****  *****  *****  *****  TwinSlider control  *****  *****  *****  *****  *)

	PROCEDURE PaintTwin (c: TwinSlider; f: Views.Frame);
		CONST
			aCol = Ports.grey25;
			bCol = Ports.grey25;
		VAR
			item: Meta.Item;
			w, h, abMax, a, aPos, b, bPos: INTEGER;
			length: LONGINT;
	BEGIN
		c.context.GetSize(w, h);
		IF ~c.disabled & c.item.Valid() THEN
			c.item.Lookup('abMax', item); abMax := item.IntVal();
			c.item.Lookup('a', item); a := item.IntVal(); Constrain(a, 0, abMax);
			c.item.Lookup('b', item); b := item.IntVal(); Constrain(b, a, abMax);
			IF w > h THEN
				length := w - 2 * f.dot;
				aPos := f.dot + SHORT(length * a DIV abMax);
				bPos := f.dot + SHORT(length * b DIV abMax);
				IF a > 0 THEN
				f.DrawRect(f.dot, f.dot, aPos, h - f.dot, fill, aCol) END;
				IF a < b THEN f.DrawRect(aPos, f.dot, bPos, h - f.dot, fill, Ports.background) END;
				IF b < abMax THEN f.DrawRect(bPos, f.dot, w - f.dot, h - f.dot, fill, bCol) END
			ELSE
				length := h - 2 * f.dot;
				aPos := f.dot + SHORT(length * (abMax - a) DIV abMax);
				bPos := f.dot + SHORT(length * (abMax - b) DIV abMax);
				IF a > 0 THEN f.DrawRect(f.dot, aPos, w - f.dot, h - f.dot, fill, aCol) END;
				IF a < b THEN f.DrawRect(f.dot, bPos, w - f.dot, aPos, fill, Ports.background) END;
				IF b < abMax THEN f.DrawRect(f.dot, f.dot, w - f.dot, bPos, fill, bCol) END
			END
		ELSE
			f.DrawRect(f.dot, f.dot, w - f.dot, h - f.dot, fill, Ports.grey50)
		END;
		f.DrawRect(0, 0, w, h, f.dot, Ports.defaultColor)
	END PaintTwin;


	PROCEDURE TrackTwin (c: TwinSlider; f: Views.Frame; VAR msg: Controllers.TrackMsg);
		VAR
			abMax: LONGINT;
			abM, a, b, aOld, bOld, g, n, x, y, w, h, a2b: INTEGER;
			aItem, bItem, gItem: Meta.Item;
			m: SET;
			horiz, isDown, aFlg, ctlFlg: BOOLEAN;
	BEGIN
		c.item.Lookup('abMax', aItem); abM := aItem.IntVal(); abMax := abM;
		c.item.Lookup('a', aItem); aOld := aItem.IntVal();
		c.item.Lookup('b', bItem); bOld := bItem.IntVal();
		c.item.Lookup('minGap', gItem); g := gItem.IntVal();
		Constrain(g, 0, abM);
		c.context.GetSize(w, h);
		horiz := w > h;
		f.Input(x, y, m, isDown); ctlFlg := Controllers.modify IN msg.modifiers;
		IF horiz THEN DEC(w, 2 * f.dot); n := SHORT(x * abMax DIV w)
		ELSE DEC(h, 2 * f.dot); n := SHORT(abMax - y * abMax DIV h) END;
		aFlg := (n + n) <= (aOld + bOld); a2b := bOld - aOld;
		REPEAT
			f.Input(x, y, m, isDown);
			ctlFlg := ctlFlg & (Controllers.modify IN msg.modifiers);
			IF horiz THEN n := SHORT(x * abMax DIV w)
			ELSE n := SHORT(abMax - y * abMax DIV h) END;
			IF aFlg THEN
				a := MIN(n, abM - g);
				IF ctlFlg THEN b := MAX(g, MIN(a + a2b, abM))
				ELSE b := MAX(a + g, bOld) END
			ELSE
				b := MAX(n, g);
				IF ctlFlg THEN a := MIN(abM - g, MAX(b - a2b, 0))
				ELSE a := MIN(aOld, b - g) END
			END;
			Constrain(a, 0, abM);
			Constrain(b, a, abM);
			IF (a # aOld) OR (b # bOld) THEN
				aItem.PutIntVal(a); aOld := a;
				bItem.PutIntVal(b); bOld := b;
				PaintTwin(c, f);
				Controls.Notify(c, f, Dialog.changed, 1, 2)
			END
		UNTIL ~isDown
	END TrackTwin;


	PROCEDURE (c: TwinSlider) Internalize2 (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		rd.ReadVersion(minVersion, maxVersion, version)
	END Internalize2;


	PROCEDURE (c: TwinSlider) Externalize2 (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion)
	END Externalize2;


	PROCEDURE (c: TwinSlider) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		PaintTwin(c, f)
	END Restore;


	PROCEDURE (c: TwinSlider) HandleCtrlMsg2 (f: Views.Frame;
	VAR msg: Controllers.Message; VAR focus: Views.View);
	BEGIN
		IF ~c.disabled & ~c.readOnly & c.item.Valid() THEN
			WITH msg: Controllers.PollCursorMsg DO msg.cursor := Ports.graphicsCursor
			| msg: Controllers.TrackMsg DO TrackTwin(c, f, msg)
			ELSE
			END
		END
	END HandleCtrlMsg2;


	PROCEDURE (c: TwinSlider) HandlePropMsg2 (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.FocusPref DO
			IF ~c.disabled & ~c.readOnly THEN msg.setFocus := TRUE END
		| msg: Controls.PropPref DO EXCL(msg.valid, Controls.label)
		| msg: Properties.SizePref DO
			IF msg.w > msg.h THEN msg.w := MAX(25 * Ports.mm, msg.w)
			ELSE msg.h := MAX(25 * Ports.mm, msg.h) END;
			msg.w := MAX(4 * Ports.mm, msg.w);
			msg.h := MAX(4 * Ports.mm, msg.h)
		ELSE
		END
	END HandlePropMsg2;


	PROCEDURE (c: TwinSlider) CheckLink (VAR ok: BOOLEAN);
		VAR mod, name: Meta.Name;
	BEGIN
		ok := FALSE;
		IF c.item.typ = Meta.recTyp THEN
			c.item.GetTypeName(mod, name);
			ok := (mod$ = 'CtlsSliders') & (name$ = 'Pair')
		END
	END CheckLink;


	PROCEDURE (c: TwinSlider) Update (f: Views.Frame; op, from, to: INTEGER);
	BEGIN
		PaintTwin(c, f)
	END Update;


	PROCEDURE NewTwinSlider* (p: Controls.Prop): Controls.Control;
		VAR c: TwinSlider;
	BEGIN
		NEW(c); Controls.OpenLink(c, p); RETURN c
	END NewTwinSlider;


	PROCEDURE DepositTwinSlider*;
		VAR p: Controls.Prop;
	BEGIN
		NEW(p); Views.Deposit(NewTwinSlider(p))
	END DepositTwinSlider;

	(*  *****  *****  *****  *****  Character control  *****  *****  *****  *****  *)

	PROCEDURE GetField (f: StdCFrames.Field; OUT str: ARRAY OF CHAR);
		VAR c: CharField;
	BEGIN
		c := f.view(CharField);
		IF c.item.Valid() & (c.item.typ = Meta.charTyp) THEN str[0] := c.item.CharVal(); str[1] := 0X END
	END GetField;


	PROCEDURE SetField (f: StdCFrames.Field; IN str: ARRAY OF CHAR);
		VAR c: CharField;
	BEGIN
		c := f.view(CharField);
		IF c.item.typ = Meta.charTyp THEN c.item.PutCharVal(str[0]); Controls.Notify(c, f, Dialog.changed, 0, 0) END
	END SetField;


	PROCEDURE EqualField (f: StdCFrames.Field; IN s1, s2: ARRAY OF CHAR): BOOLEAN;
	BEGIN
		RETURN s1 = s2
	END EqualField;


	PROCEDURE (c: CharField) Internalize2 (VAR rd: Stores.Reader);
		VAR version: INTEGER;
	BEGIN
		IF ~rd.cancelled THEN rd.ReadVersion(minVersion, maxVersion, version) END
	END Internalize2;


	PROCEDURE (c: CharField) Externalize2 (VAR wr: Stores.Writer);
	BEGIN
		wr.WriteVersion(maxVersion)
	END Externalize2;


	PROCEDURE (c: CharField) GetNewFrame (VAR frame: Views.Frame);
		VAR f: StdCFrames.Field;
	BEGIN
		f := StdCFrames.dir.NewField();
		f.disabled := c.disabled;
		f.undef := c.undef;
		f.readOnly := c.readOnly;
		f.font := c.font;
		f.maxLen := maxLen;
		f.Get := GetField;
		f.Set := SetField;
		f.Equal := EqualField;
		frame := f
	END GetNewFrame;


	PROCEDURE (c: CharField) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		WITH f: StdCFrames.Frame DO f.Restore(l, t, r, b) END
	END Restore;


	PROCEDURE (c: CharField) HandleCtrlMsg2 (f: Views.Frame;
	VAR msg: Controllers.Message; VAR focus: Views.View);
		VAR ch: CHAR; a, b: INTEGER;
	BEGIN
		IF ~c.disabled & ~c.readOnly THEN
			WITH f: StdCFrames.Field DO
				WITH msg: Controllers.PollOpsMsg DO
					msg.valid := {Controllers.copy}
				| msg: Controllers.EditMsg DO
					IF msg.op = Controllers.pasteChar THEN
						ch := msg.char;
						IF c.item.typ = Meta.charTyp THEN f.KeyDown(ch, {}); f.Select(0, 1); f.Update END
					ELSE
						f.Edit(msg.op, msg.view, msg.w, msg.h, msg.isSingle, msg.clipboard)
					END
				| msg: Controllers.PollCursorMsg DO
					f.GetCursor(msg.x, msg.y, msg.modifiers, msg.cursor)
				| msg: Controllers.TrackMsg DO
					f.MouseDown(msg.x, msg.y, msg.modifiers);
					f.GetSelection(a, b)
				| msg: Controllers.MarkMsg DO
					f.Mark(msg.show, msg.focus);
					IF msg.focus THEN
						IF msg.show
							THEN f.Select(0, 256)	(* set selection on focus *)
							ELSE f.Select( - 1, - 1); f.Update END	(* remove selection on defocus *)
					END
				ELSE
				END
			END
		END
	END HandleCtrlMsg2;


	PROCEDURE (c: CharField) HandlePropMsg2 (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.FocusPref DO
			IF ~c.disabled & ~c.readOnly THEN msg.setFocus := TRUE END
		| msg: Properties.SizePref DO
			StdCFrames.dir.GetFieldSize(maxLen, msg.w, msg.h)
		| msg: Controls.PropPref DO
			msg.valid := msg.valid + {Controls.level}
		ELSE
		END
	END HandlePropMsg2;


	PROCEDURE (c: CharField) CheckLink (VAR ok: BOOLEAN);
	BEGIN
		ok := c.item.typ = Meta.charTyp
	END CheckLink;


	PROCEDURE (c: CharField) Update (f: Views.Frame; op, from, to: INTEGER);
	BEGIN
		f(StdCFrames.Frame).Update
	END Update;


	PROCEDURE NewCharField* (p: Controls.Prop): Controls.Control;
		VAR c: CharField;
	BEGIN
		NEW(c); Controls.OpenLink(c, p); RETURN c
	END NewCharField;


	PROCEDURE DepositCharField*;
		VAR p: Controls.Prop;
	BEGIN
		NEW(p); Views.Deposit(NewCharField(p))
	END DepositCharField;


BEGIN
	arial8pt := Fonts.dir.This('Arial', 8 * Fonts.point, {}, Fonts.normal);
	pair.a := 25; pair.b := 55; pair.minGap := 5; pair.abMax := 100
END NetSliders.


DevDebug.Unload

"NetSliders.DepositSlider; StdCmds.PasteView"

