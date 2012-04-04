// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionValue;
import com.amazon.fusion.Language;
import com.amazon.ion.IonException;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Iterator;

/**
 *
 */
final class Eval
    extends Command
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "Evaluate a file.";
    private static final String HELP_USAGE =
        "eval FILE";
    private static final String HELP_BODY =
        "Evaluates the Fusion script in the given FILE.";


    private final IonSystem mySystem = IonSystemBuilder.standard().build();
    private final Language myLanguage = new Language();
    private String myFileName;

    Eval()
    {
        super("eval");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }

    @Override
    boolean processArguments(String[] args)
    {
        boolean ok = (args.length == 1);
        if (ok)
        {
            myFileName = args[0];
            ok = (myFileName.length() != 0);
        }
        return ok;
    }

    @Override
    void execute()
    {
        try
        {
            FusionValue result = evalFile(myFileName);
            if (result != FusionValue.UNDEF)
            {
                myLanguage.writeToStdout(result);
            }
        }
        catch (IOException e)
        {
            throw new IonException(e);
        }
    }


    /**
     * @return not null.
     */
    private FusionValue evalFile(String fileName)
        throws IOException
    {
        FileInputStream in = new FileInputStream(fileName);
        try
        {
            Iterator<IonValue> i = mySystem.iterate(in);
            return myLanguage.eval(i);
        }
        finally
        {
            in.close();
        }
    }
}
