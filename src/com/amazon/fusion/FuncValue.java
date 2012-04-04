// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;

/**
 * A user-defined function, the result of evaluating a {@link FuncKeyword}.
 */
final class FuncValue
    extends FunctionValue
{
    private final Environment myEnclosure;
    private final IonSexp myDefinition;
    private final String[] myParams;
    private final String myDoc;
    private final IonValue myBody;

    /**
     * Constructs a new function from its source and enclosing lexical
     * environment.
     *
     * @param definition the source text of the {@code func} expression.
     * @param enclosure the lexical environment surrounding the source of this
     *  function.  Any free variables in the function are expected to be bound
     *  here.
     */
    FuncValue(IonSexp definition, Environment enclosure)
    {
        myEnclosure = enclosure;
        myDefinition = definition;
        myParams = determineParams((IonSexp) definition.get(1));

        IonValue maybeDoc = definition.get(2);
        if (maybeDoc.getType() == IonType.STRING
            && definition.size() > 3)
        {
            myDoc = ((IonString) maybeDoc).stringValue();
            myBody = definition.get(3);
        }
        else
        {
            myDoc = null;
            myBody = maybeDoc;
        }
    }

    private String[] determineParams(IonSexp paramsExpr)
    {
        int size = paramsExpr.size();
        String[] params = new String[size];
        for (int i = 0; i < size; i++)
        {
            IonSymbol param = (IonSymbol) paramsExpr.get(i);
            params[i] = param.stringValue();
        }
        return params;
    }

    @Override
    void printDoc(Writer out)
        throws IOException
    {
        if (myDoc != null)
        {
            out.write(myDoc);
            if (! myDoc.endsWith("\n"))
            {
                out.write('\n');
            }
        }
        else
        {
            super.printDoc(out);
        }
    }


    @Override
    IonValue getDom()
    {
        return myDefinition;
    }

    @Override
    void print(Writer out)
        throws IOException
    {
        out.write(myDefinition.toString());
    }

    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
    {
        if (myParams.length != args.length)
        {
            throw new RuntimeException("Bad application of\n  " + myDefinition +
                                       "\nto\n  " + Arrays.toString(args));
        }

        Environment c2 = new Environment()
        {
            @Override
            public FusionValue lookup(String name)
            {
                for (int i = 0; i < myParams.length; i++)
                {
                    if (name.equals(myParams[i]))
                    {
                        return args[i];
                    }
                }

                return myEnclosure.lookup(name);
            }

            @Override
            public void collectNames(Collection<String> names)
            {
                for (String name : myParams)
                {
                    names.add(name);
                }
                myEnclosure.collectNames(names);
            }
        };

        return eval.eval(c2, myBody);
    }
}
