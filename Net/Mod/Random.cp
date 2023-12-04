MODULE NetRandom;

	IMPORT Services;

	TYPE
		RandomGenerator* = POINTER TO RECORD
			z: INTEGER
		END;

	VAR
		gen: RandomGenerator;

	PROCEDURE (rand: RandomGenerator) Uniform* (): REAL, NEW;
		CONST a = 16807; m = 2147483647; q = m DIV a; r = m MOD a;
		VAR gamma: INTEGER;
	BEGIN
		gamma := a * (rand.z MOD q) - r * (rand.z DIV q);
		IF gamma > 0 THEN
			rand.z := gamma
		ELSE
			rand.z := gamma + m
		END;
		RETURN rand.z / m
	END Uniform;

	PROCEDURE New* (): RandomGenerator;
		VAR rand: RandomGenerator;
	BEGIN
		NEW(rand);
		rand.z := SHORT(ENTIER(gen.Uniform() * 1000000));
		RETURN rand
	END New;
	
BEGIN
	NEW(gen);
	gen.z := SHORT(Services.Ticks() MOD 2147483647);
	
END NetRandom.
