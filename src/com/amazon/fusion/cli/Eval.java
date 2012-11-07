// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.ExitException;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion.FusionValue;
import com.amazon.fusion.SourceName;
import com.amazon.fusion.TopLevel;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.FileInputStream;
import java.io.IOException;

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

    Eval()
    {
        super("eval");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }

    @Override
    Executor processArguments(String[] args)
    {
        if (args.length != 1) return null;

        String fileName = args[0];
        if (fileName.length() == 0) return null;

        return new Executor(fileName);
    }


    private static class Executor
        implements Command.Executor
    {
        private final IonSystem     mySystem;
        private final FusionRuntime myRuntime;
        private final String        myFileName;


        private Executor(String fileName)
        {
            mySystem   = IonSystemBuilder.standard().build();
            myRuntime  = FusionRuntimeBuilder.standard().build();
            myFileName = fileName;
        }


        @Override
        public int execute()
            throws Exception
        {
            try
            {
                Object result = evalFile(myFileName);
                if (result != FusionValue.UNDEF)
                {
                    FusionValue.write(System.out, result);
                    System.out.println();
                }
            }
            catch (ExitException e)
            {
                // Do nothing, just return successfully.
            }
            catch (FusionException e)
            {
                System.err.println(e.getMessage());
                return 1;
            }

            return 0;
        }


        /**
         * @return not null.
         */
        private Object evalFile(String fileName)
            throws FusionException, IOException
        {
            TopLevel top = myRuntime.getDefaultTopLevel();

            SourceName name = SourceName.forFile(fileName);
            FileInputStream in = new FileInputStream(fileName);
            try
            {
                IonReader i = mySystem.newReader(in);
                return top.eval(i, name);
            }
            finally
            {
                in.close();
            }
        }
    }
}
