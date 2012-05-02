// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonValue;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Iterator;

/**
 * This is syntax because we currently evaluate the file in the invoker's
 * environment. That's not right but there's no access on anything else now.
 */
class EvalFileKeyword
    extends KeywordValue
{
    private final DynamicParameter myCurrentDirectoryParameter;

    EvalFileKeyword(DynamicParameter currentDirectoryParameter)
    {
        //    "                                                                               |
        super("FILENAME",
              "Opens the Fusion source file named by the given string and evaluates each\n" +
              "expression in sequence. Returns the last result.\n" +
              "FILENAME is resolve relative to the value of current_directory.");

        myCurrentDirectoryParameter = currentDirectoryParameter;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        String fileName;
        {
            IonValue argExpr = expr.get(1);
            FusionValue argValue = eval.eval(env, argExpr);
            IonString nameDom = (IonString) ((DomValue) argValue).getDom();
            fileName = nameDom.stringValue();
        }

        return evalFile(eval, env, fileName);
    }


    FusionValue evalFile(Evaluator eval, Environment env, String path)
        throws FusionException
    {
        // TODO error handling
        FusionValue cd = eval.applyNonTail(myCurrentDirectoryParameter);
        String cdPath = ((IonString) ((DomValue) cd).getDom()).stringValue();
        File cdFile = new File(cdPath);

        File evaluatedFile = new File(cdFile, path);
        try
        {
            FileInputStream in = new FileInputStream(evaluatedFile);
            try
            {
                FusionValue result = null;

                Iterator<IonValue> i = eval.getSystem().iterate(in);
                while (i.hasNext())
                {
                    IonValue fileExpr = i.next();
                    result = eval.eval(env, fileExpr);
                }

                return result;
            }
            finally
            {
                in.close();
            }
        }
        catch (IOException e)
        {
            throw new FusionException(e);
        }
    }
}
