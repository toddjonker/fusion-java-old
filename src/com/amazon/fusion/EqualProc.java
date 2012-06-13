// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonValue;

/**
 *
 */
final class EqualProc
    extends Procedure
{
    EqualProc()
    {
        //    "                                                                               |
        super("Returns true if the arguments are equal integers, false otherwise.",
              "int", "int");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
    	IonValue leftVal = args[0].ionValue();
    	IonValue rightVal = args[1].ionValue();
    	boolean result = false;
    	IonBool resultDom;

    	if (leftVal instanceof IonInt && rightVal instanceof IonInt)
    	{
            IonInt left  = (IonInt) leftVal;
            IonInt right = (IonInt) rightVal;
            result = left.longValue() == right.longValue();
            resultDom = left.getSystem().newBool(result);
    	} else if (leftVal instanceof IonBool && rightVal instanceof IonBool)
    	{
            IonBool left  = (IonBool) leftVal;
            IonBool right = (IonBool) rightVal;
            result = left.booleanValue() == right.booleanValue();
            resultDom = left.getSystem().newBool(result);
    	} else
    	{
    		throw new ContractFailure("Mismatched or invalid type equivalence cannot be determined.");
    	}

        return new DomValue(resultDom);
    }
}
