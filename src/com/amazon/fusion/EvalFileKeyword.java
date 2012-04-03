// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonValue;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Iterator;

/**
 *
 */
class EvalFileKeyword
    extends KeywordValue
{
    EvalFileKeyword()
    {
        super("eval_file", "FILENAME",
              "Opens the Fusion source file named by the given string and evaluates\n" +
              "each expression in sequence. Returns the last result.");
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        String fileName;
        {
            IonValue argExpr = expr.get(1);
            FusionValue argValue = eval.eval(env, argExpr);
            IonString nameDom = (IonString) ((DomValue) argValue).getDom();
            fileName = nameDom.stringValue();
        }

        try
        {
            FileInputStream in = new FileInputStream(fileName);
            try
            {
                FusionValue result = null;

                Iterator<IonValue> i = expr.getSystem().iterate(in);
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
            throw new IonException(e);
        }
    }
}
