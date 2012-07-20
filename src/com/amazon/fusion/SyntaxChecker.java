// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Helper class for checking syntactic forms.
 */
final class SyntaxChecker
{
    private final String myFormName;
    private final SyntaxSexp myForm;

    SyntaxChecker(String formName, SyntaxSexp form)
    {
        myFormName = formName;
        myForm = form;
    }


    //========================================================================

    final SyntaxFailure failure(String message)
    {
        return new SyntaxFailure(myFormName, message, myForm);
    }

    /**
     * Arity includes the initial keyword!
     */
    final void arityExact(int count)
        throws SyntaxFailure
    {
        if (myForm.size() != count)
        {
            throw failure("expected " + (count-1) + " subforms");
        }
    }

    /**
     * Arity includes the initial keyword!
     */
    final void arityAtLeast(int count)
        throws SyntaxFailure
    {
        if (myForm.size() < count)
        {
            throw failure("expect at least " + (count-1) + " subforms");
        }
    }


    final SyntaxValue requiredForm(String expectation, int argNum)
        throws SyntaxFailure
    {
        try
        {
            return myForm.get(argNum);
        }
        catch (IndexOutOfBoundsException e) {}

        throw failure("expected " + expectation);
    }


    final SyntaxSequence requiredSequence(String expectation, int argNum)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum);
        return checkSyntax(SyntaxSequence.class, expectation,
                           false /* nullable */, form);
    }


    final String requiredString(String expectation, int argNum)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum);
        return checkSyntax(SyntaxString.class, expectation,
                           false /* nullable */, form).stringValue();
    }


    final String requiredNonEmptyString(String expectation, int argNum)
        throws SyntaxFailure
    {
        String str = requiredString(expectation, argNum);
        // TODO check emptyness
        return str;
    }



    final SyntaxSymbol requiredSymbol(String expectation, int argNum)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum);
        return checkSyntax(SyntaxSymbol.class, expectation,
                           false /* nullable */, form);
    }

    final String requiredNonEmptySymbol(String expectation, int argNum)
        throws SyntaxFailure
    {
        SyntaxSymbol sym = requiredSymbol(expectation, argNum);
        // TODO check emptyness
        return sym.stringValue();
    }


    final <T extends SyntaxValue> T checkSyntax(Class<T> klass,
                                               String expectation,
                                               boolean nullable,
                                               SyntaxValue form)
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

        throw new SyntaxFailure(myFormName,
                                "expected " + expectation, form);
    }
}
