// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;


/**
 * Accepts a input struct and a sequence of string args that serve as field names
 * and constructs a struct with copies of values from an input struct that match in
 * field names as specified by the user.
 *
 * The user may specify as many field names as desired, as long as the original input
 * struct contains those field names. Essentially, the resulting struct is a subset of
 * the original struct or a copy of the original struct itself if all field names are
 * provided.
 */
final class StructPruneProc
    extends Procedure
{
    StructPruneProc()
    {
        //    "                                                                               |
        super("Constructs a struct with copies of values from an input struct with " +
              "matching filed names that are specified by the user",
              "struct", "fields", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1,args);

        IonStruct result = eval.getSystem().newEmptyStruct();

        IonStruct original = checkStructArg(0, args);

        if (args.length > 1)
        {
            String key;
            for (int i = 1; i < args.length; i++)
            {
                key = checkTextArg(i, args);
                IonValue value = original.get(key);
                try
                {
                    result.put(key, value.clone()); // value can be null, so NPE possible
                } catch (NullPointerException e)
                {
                    throw contractFailure("Error: Failed to find field in struct: "+key);
                }
            }
        }

        return new DomValue(result);
    }
}
