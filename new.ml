PROGRAM minSumOfFactors 
{ 
num Integer; 
sum Integer;
i Integer;
num = 12;
sum = 0;
i = 2;
	IF (num Gt 0) THEN 
	{ 
		WHILE (i Times i Le num) 
		{
			WHILE ((num Minus i Times (num Div i)) Eq 0)
			{ 
				sum = sum Plus i; 
				num = num Div i;
			}
				i = i Plus 1;
		} 
	sum = sum Plus num;
	}
	ELSE 
	{
		sum = 0;
	}
}
