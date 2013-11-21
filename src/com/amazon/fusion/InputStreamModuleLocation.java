// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import java.io.IOException;
import java.io.InputStream;

/**
 *
 */
abstract class InputStreamModuleLocation
    extends ModuleLocation
{
    abstract InputStream open()
        throws IOException;


    @Override
    IonReader read(Evaluator eval)
        throws IOException
    {
        IonReader reader = null;
        InputStream in = open();
        try
        {
            reader = eval.getSystem().newReader(in);
            return reader;
        }
        finally
        {
            if (reader == null)
            {
                // We failed constructing the IonReader!
                in.close();
            }
        }
    }
}
