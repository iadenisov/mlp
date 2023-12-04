MODULE NetLanguage;

	IMPORT Dialog, Properties, NetMain;
	
	VAR
		lang: Dialog.Language; 

	PROCEDURE SetLang*(l: ARRAY OF CHAR);
		VAR res: INTEGER;
	BEGIN
		Dialog.SetLanguage(l$, TRUE);
		lang := l$;
		Dialog.Call("StdMenuTool.UpdateAllMenus", "", res);
		NetMain.UpdateLang;
	END SetLang;
	
	PROCEDURE LangGuard*(l: INTEGER; VAR par: Dialog.Par);
	BEGIN
		CASE l OF
			1: par.checked := lang = 'en';
			|
			2: par.checked := lang = 'ru';
		END;
	END LangGuard;

BEGIN
	lang := Dialog.language;
END NetLanguage.
