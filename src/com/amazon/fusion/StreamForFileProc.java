// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Iterator;

final class StreamForFileProc
    extends Procedure
{
    StreamForFileProc()
    {
        super("Opens the file identified by PATH, returning a stream of Ion values.",
              "path");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        String path = checkTextArg(0, args);

        IonSystem system = eval.getSystem();
        FileInputStream in;
        try
        {
            in = new FileInputStream(path);
        }
        catch (FileNotFoundException e)
        {
            throw contractFailure("File not found: " + path);
        }

        Iterator<IonValue> iter = system.iterate(in);
        return Sequences.streamFor(iter);
    }
}
