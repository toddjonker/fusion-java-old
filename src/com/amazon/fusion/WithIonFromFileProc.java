// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

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
        //    "                                                                               |
        super("Opens the file at `path` and uses it as the current Ion input stream while\n" +
              "applying the `thunk`.  The file is closed when the thunk returns (normally or\n" +
              "abnormally).\n" +
              "\n" +
              "The `path` must be a non-empty string denoting the file to read.  If the path\n" +
              "is relative, it is resolved against `current_directory`.  The `thunk` must be a\n" +
              "procedure that accepts zero arguments.\n" +
              "\n" +
              "A common use of this procedure is to read a single Ion value from a file:\n" +
              "\n" +
              "    (with_ion_from_file \"path/to/file.ion\" read)\n" +
              "\n" +
              "This leverages the fact that `read` accepts zero arguments and consumes the\n" +
              "current Ion input stream.",
              "path", "thunk");

        myCurrentDirectoryParam = (DynamicParameter) currentDirectoryParam;
        myCurrentIonReaderParam = (DynamicParameter) currentIonReaderParam;
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        String path = checkNonEmptyStringArg(eval, this, 0, args);

        Procedure thunk = checkProcArg(1, args);
        // TODO FUSION-85 check thunk arity

        try
        {
            File inFile = resolvePath(eval, myCurrentDirectoryParam, path);
            FileInputStream in = new FileInputStream(inFile);
            try
            {
                IonReader reader = eval.getSystem().newReader(in);
                Evaluator parameterized =
                    eval.markedContinuation(myCurrentIonReaderParam, reader);

                // This looks like its a tail call, but its not!  We must not
                // close the stream until after the call returns.
                return parameterized.callNonTail(thunk);
            }
            finally
            {
                in.close();
            }
        }
        catch (IOException e)
        {
            // TODO improve error message
            throw new FusionException(e);
        }
    }
}
