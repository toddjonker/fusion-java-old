// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;

/**
 * Base class for syntactic forms
 * (as opposed to {@linkplain FunctionValue functions}).
 */
abstract class KeywordValue
    extends NamedValue
{
    protected final String myBodyPattern;
    protected final String myDoc;

    KeywordValue(String bodyPattern, String doc)
    {
        myBodyPattern = bodyPattern;
        myDoc = doc;
    }

    @Override
    abstract FusionValue invoke(Evaluator eval,
                                Environment env,
                                IonSexp expr)
        throws FusionException;


    @Override
    final void identify(Appendable out)
        throws IOException
    {
        String name = getInferredName();
        if (name == null)
        {
            out.append("anonymous syntax");
        }
        else
        {
            out.append("keyword ");
            IonTextUtils.printQuotedSymbol(out, name);
        }
    }


    @Override
    void displayHelp(Appendable out)
        throws IOException
    {
        out.append("[SYNTAX]  (");
        out.append(getEffectiveName());
        if (myBodyPattern != null)
        {
            out.append(' ');
            out.append(myBodyPattern);
        }
        out.append(")\n\n");
        out.append(myDoc);
        out.append('\n');
    }


    //========================================================================
    // Type-checking helpers


    final IonValue requiredForm(String expectation,
                                int argNum,
                                IonSequence parent)
        throws SyntaxFailure
    {
        try
        {
            return parent.get(argNum);
        }
        catch (IndexOutOfBoundsException e) {}

        throw new SyntaxFailure(getEffectiveName(),
                                "expected " + expectation,
                                parent);
    }


    final IonSymbol requiredSymbol(String expectation,
                                   int argNum,
                                   IonSequence parent)
        throws SyntaxFailure
    {
        IonValue form = requiredForm(expectation, argNum, parent);
        return checkSyntax(IonSymbol.class, expectation,
                           false /* nullable */, form);
    }

    final IonSymbol requiredSymbol(String expectation, IonValue arg)
        throws SyntaxFailure
    {
        return checkSyntax(IonSymbol.class, expectation,
                           false /* nullable */, arg);
    }


    final IonSequence requiredSequence(String expectation,
                                       int argNum,
                                       IonSequence parent)
        throws SyntaxFailure
    {
        IonValue form = requiredForm(expectation, argNum, parent);
        return checkSyntax(IonSequence.class, expectation,
                           false /* nullable */, form);
    }

    final IonSequence requiredSequence(String expectation, IonValue arg)
        throws SyntaxFailure
    {
        return checkSyntax(IonSequence.class, expectation,
                           false /* nullable */, arg);
    }


    final IonSexp requiredSexp(String expectation,
                               int argNum,
                               IonSequence parent)
        throws SyntaxFailure
    {
        IonValue form = requiredForm(expectation, argNum, parent);
        return checkSyntax(IonSexp.class, expectation,
                           false /* nullable */, form);
    }

    final IonSexp requiredSexp(String expectation, IonValue arg)
        throws SyntaxFailure
    {
        return checkSyntax(IonSexp.class, expectation,
                           false /* nullable */, arg);
    }


    final <T extends IonValue> T checkSyntax(Class<T> klass,
                                             String expectation,
                                             boolean nullable,
                                             IonValue form)
        throws SyntaxFailure
    {
        try
        {
            if (nullable || ! form.isNullValue())
            {
                return klass.cast(form);
            }
        }
        catch (ClassCastException e) {}

        throw new SyntaxFailure(this.getEffectiveName(),
                                "expected " + expectation, form);
    }
}
