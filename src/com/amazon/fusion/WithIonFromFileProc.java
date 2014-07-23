// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.checkNonEmptyStringArg;
import static com.amazon.fusion.FusionUtils.resolvePath;
import com.amazon.ion.IonReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

/**
 *
 */
final class WithIonFromFileProc
    extends Procedure
{
    private final DynamicParameter myCurrentDirectoryParam;
    private final DynamicParameter myCurrentIonReaderParam;


    public WithIonFromFileProc(Object currentDirectoryParam,
                               Object currentIonReaderParam)
    {
        myCurrentDirectoryParam = (DynamicParameter) currentDirectoryParam;
        myCurrentIonReaderParam = (DynamicParameter) currentIonReaderParam;
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        String path = checkNonEmptyStringArg(eval, this, 0, args);

        Procedure thunk = checkProcArg(1, args);
        // TODO FUSION-85 check thunk arity

        try
        {
            File inFile = resolvePath(eval, myCurrentDirectoryParam, path);

            try (FileInputStream in = new FileInputStream(inFile))
            {
                IonReader reader = eval.getSystem().newReader(in);
                Evaluator parameterized =
                    eval.markedContinuation(myCurrentIonReaderParam, reader);

                // We cannot use a tail call here, since we must not close the
                // stream until after the call returns.
                return parameterized.callNonTail(thunk);
            }
        }
        catch (IOException e)
        {
            // TODO improve error message
            throw new FusionException(e);
        }
    }
}
