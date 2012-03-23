// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;


class FuncValue
    extends FunctionValue
{
    private final Environment myEnclosure;
    private final IonSexp myDefinition;
    private final String myParam;
    private final String myDoc;
    private final IonValue myBody;

    FuncValue(IonSexp definition, Environment enclosure)
    {
        myDefinition = definition;

        myEnclosure = enclosure;

        IonSymbol param = (IonSymbol) definition.get(1);
        myParam = param.stringValue();

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
    FusionValue invoke(Evaluator eval, final FusionValue argumentValue)
    {
        Environment c2 = new Environment()
        {
            @Override
            public FusionValue lookup(String name)
            {
                if (name.equals(myParam))
                {
                    return argumentValue;
                }

                return myEnclosure.lookup(name);
            }

            @Override
            public void collectNames(Collection<String> names)
            {
                names.add(myParam);
                myEnclosure.collectNames(names);
            }
        };

        return eval.eval(c2, myBody);
    }
}
