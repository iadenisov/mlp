MODULE NetPlotMap;

	IMPORT WinApi, M := Math, Views, Ports, Properties, HostFonts := WinFonts, Fonts, HostPorts := WinPorts, Strings, Stores, Dialog;

	CONST
		shift = 50;

	TYPE
		Map = POINTER TO RECORD (Views.View)
			map: POINTER TO ARRAY OF ARRAY OF REAL;
			t1, t2: ARRAY 128 OF CHAR;
			in1min, in1max, in2min, in2max: REAL
		END;
	
	VAR font: Fonts.Font;

	PROCEDURE DrawStringA (f: Views.Frame; x, y, angle: INTEGER; col: Ports.Color; s: ARRAY OF CHAR; font: Fonts.Font);
		VAR res: INTEGER; df: HostFonts.DevFont; pt: HostPorts.Port; dc: WinApi.HANDLE; NewFont: WinApi.HFONT;
	BEGIN
		pt := f.rider(HostPorts.Rider).port;
		dc := pt.dc;
		res := WinApi.SaveDC(dc);
		res := WinApi.SetBkMode(dc, WinApi.TRANSPARENT);
		res := WinApi.SetTextColor(dc, col);
		IF angle MOD 360 = 0 THEN
			(* Если угол поворота кратен 360, то использовать обычный шрифт *)
			df := font(HostFonts.Font).dev;
			res := WinApi.SelectObject(dc, df.id);
			res := WinApi.TextOutW(dc, x, y, s, LEN(s$));
			res := WinApi.RestoreDC(dc, - 1)
		ELSE
			(* Иначе, создать новый шрифт *)
			NewFont := WinApi.CreateFontW( - ((font.size + pt.unit DIV 2) DIV pt.unit), 0, angle * 10, angle * 10, Fonts.normal, 0, 0, 0, 0, 7, 2, 1, 38, font.typeface);
			(* Выбрать шрифт *)
			res := WinApi.SelectObject(dc, NewFont);
			(* Вывести текст *)
			res := WinApi.TextOutW(dc, f.l + (f.gx + x * f.dot) DIV f.unit, f.t + (f.gy + y * f.dot) DIV f.unit, s, LEN(s$));
			res := WinApi.RestoreDC(dc, - 1);
			(* Удалить шрифт *)
			res := WinApi.DeleteObject(NewFont)
		END
	END DrawStringA;

	PROCEDURE (v: Map) CopyFromSimpleView- (source: Views.View);
	BEGIN
		WITH source: Map DO
			v.map := source.map;
			v.t1 := source.t1;
			v.t2 := source.t2;
			v.in1min := source.in1min;
			v.in1max := source.in1max;
			v.in2min := source.in2min;
			v.in2max := source.in2max;
		END
	END CopyFromSimpleView;
	
		
	PROCEDURE (m: Map) Externalize- (VAR wr: Stores.Writer);
		VAR i, j, k: INTEGER;
	BEGIN
		wr.WriteReal(m.in1min);
		wr.WriteReal(m.in1max);
		wr.WriteReal(m.in2min);
		wr.WriteReal(m.in2max);
		wr.WriteString(m.t1);
		wr.WriteString(m.t2);
		wr.WriteInt(LEN(m.map, 0));
		wr.WriteInt(LEN(m.map, 1));
		FOR i := 0 TO LEN(m.map, 0) - 1 DO
			FOR j := 0 TO LEN(m.map, 1) - 1 DO
				wr.WriteReal(m.map[i, j])
			END
		END
	END Externalize;

	PROCEDURE (m: Map) Internalize- (VAR rd: Stores.Reader);
		VAR x, y, i, j, k: INTEGER;
	BEGIN
		rd.ReadReal(m.in1min);
		rd.ReadReal(m.in1max);
		rd.ReadReal(m.in2min);
		rd.ReadReal(m.in2max);
		rd.ReadString(m.t1);
		rd.ReadString(m.t2);
		rd.ReadInt(x);
		rd.ReadInt(y);
		NEW(m.map, x, y);
		FOR i := 0 TO LEN(m.map, 0) - 1 DO
			FOR j := 0 TO LEN(m.map, 1) - 1 DO
				rd.ReadReal(m.map[i, j])
			END
		END
	END Internalize;
	
	PROCEDURE (m: Map) Restore (f: Views.Frame; fl, ft, fr, fb: INTEGER);
	VAR i, j, width, height, col, stepx, stepy: INTEGER; max, min, scale: REAL;
		tmp: ARRAY 64 OF CHAR;
	BEGIN
		min := MAX(REAL);
		max := MIN(REAL);
		FOR i := 0 TO LEN(m.map,0)-1 DO
			FOR j := 0 TO LEN(m.map,1)-1 DO
				IF m.map[i, j] > max THEN max := m.map[i, j] END;
				IF m.map[i, j] < min THEN min := m.map[i, j] END;
			END
		END;
		scale := 254 / (max - min);
		m.context.GetSize(width, height);
		(*
		DevDebug.UnloadThis NetMain NetPlotMap NetService NetRandom NetLanguage
		*)
		FOR i := 0 TO 255 DO
			f.DrawRect((shift + i * (width DIV f.dot) DIV 255) * f.dot, height - 15 * f.dot, (shift + (i + 1) * (width DIV f.dot) DIV 255) * f.dot, height , - 1, Ports.RGBColor(i, 0, 255 - i));
		END;
		Strings.RealToStringForm(min, 4, 0, 0, " ", tmp);
		f.DrawString((shift + 10) * f.dot, height - 3 * f.dot, Ports.white, tmp, font);
		Strings.RealToStringForm(max, 4, 0, 0, " ", tmp);
		f.DrawString(width - font.StringWidth(tmp$) - 5 * f.dot, height - 3 * f.dot, Ports.white, tmp, font);
		
		stepx := (width - shift * f.dot) DIV LEN(m.map, 0);
		stepy := (height - shift * f.dot) DIV LEN(m.map, 1);
		(* t1 *)
		Strings.RealToStringForm(m.in1min, 4, 0, 0, " ", tmp);
		f.DrawString(shift * f.dot, (height DIV f.dot - shift DIV 2) * f.dot, Ports.black, tmp, font);
		f.DrawString((width - shift * f.dot + font.StringWidth(m.t1)) DIV 2, (height DIV f.dot - shift DIV 2) * f.dot, Ports.black, m.t1, font);
			Strings.RealToStringForm(m.in1max, 4, 0, 0, " ", tmp);
		f.DrawString((width - font.StringWidth(tmp)), (height DIV f.dot - shift DIV 2) * f.dot, Ports.black, tmp, font);
		
		(* t2 *)
		Strings.RealToStringForm(m.in2min, 4, 0, 0, " ", tmp);
		DrawStringA(f, (shift - font.size DIV f.dot), (height - shift * f.dot) DIV f.dot, 90, Ports.black, tmp, font);
		DrawStringA(f, (shift - font.size DIV f.dot), (height - shift * f.dot + font.StringWidth(m.t2)) DIV f.dot DIV 2, 90, Ports.black, m.t2, font);
		Strings.RealToStringForm(m.in2max, 4, 0, 0, " ", tmp);
		DrawStringA(f, (shift - font.size DIV f.dot), (font.StringWidth(tmp)) DIV f.dot, 90, Ports.black, tmp, font);
				
		FOR i := 0 TO LEN(m.map,0)-1 DO
			FOR j := 0 TO LEN(m.map,1)-1 DO
				col := SHORT(ENTIER((m.map[i, j] - min) * scale));
				f.DrawRect(
					shift * f.dot + i * stepx,
					(LEN(m.map,1) - 1 - j) * stepy,
					shift * f.dot + (i + 1) * stepx,
					((LEN(m.map,1) - j)) * stepy,
					- 1, Ports.RGBColor(col, 0, 255 - col ))
			END
		END;
		
	END Restore;
	
	PROCEDURE (v: Map) HandlePropMsg (VAR msg: Properties.Message);
		CONST w = 90 * Ports.mm; h = 90 * Ports.mm;
	BEGIN
		WITH msg: Properties.SizePref DO
			IF msg.w = Views.undefined THEN
				msg.w := w; msg.h := h
			END
		| msg: Properties.ResizePref DO
			msg.horFitToWin := TRUE; msg.verFitToWin := TRUE
	END END HandlePropMsg;
	
	PROCEDURE Plot* (VAR map: ARRAY OF ARRAY OF REAL; t1, t2: ARRAY OF CHAR;
		in1min, in1max, in2min, in2max: REAL);
		VAR m: Map; i, j: INTEGER; title: Views.Title;
	BEGIN
		NEW(m);
		m.t1 := t1$;
		m.t2 := t2$;
		m.in1min := in1min;
		m.in1max := in1max;
		m.in2min := in2min;
		m.in2max := in2max;
		NEW(m.map, LEN(map, 0), LEN(map, 1));
		FOR i := 0 TO LEN(map,0)-1 DO
			FOR j := 0 TO LEN(map,1)-1 DO
				m.map[i, j] := map[i, j]
			END
		END;
		Dialog.MapString("#Net:Map", title);
		Views.OpenAux(m, title);
	END Plot;

BEGIN
	font    := Fonts.dir.This("Verdana", 10 * Ports.point, {}, Fonts.normal);

END NetPlotMap.

DevDebug.UnloadThis NetMain NetPlotMap NetService NetRandom NetLanguage
