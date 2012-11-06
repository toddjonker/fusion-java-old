// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Helper class for checking syntactic forms.
 */
class SyntaxChecker
{
    final String myFormName;
    final SyntaxSequence myForm;

    SyntaxChecker(String formName, SyntaxSequence form)
    {
        myFormName = formName;
        myForm = form;
    }


    SyntaxSequence form()
    {
        return myForm;
    }


    //========================================================================

    SyntaxFailure failure(String message)
    {
        return new SyntaxFailure(myFormName, message, myForm);
    }

    SyntaxFailure failure(String message, SyntaxValue subform)
    {
        return new SyntaxFailure(myFormName, message, subform, myForm);
    }

    /**
     * Arity includes the initial syntactic form identifier!
     */
    void arityExact(int count)
        throws SyntaxFailure
    {
        if (myForm.size() != count)
        {
            throw failure("expected " + (count-1) + " subforms");
        }
    }

    /**
     * Arity includes the initial syntactic form identifier!
     * @return the actual arity (including the initial syntactic form
     * identifier).
     */
    final int arityAtLeast(int count)
        throws SyntaxFailure
    {
        int size = myForm.size();
        if (size < count)
        {
            throw failure("expect at least " + (count-1) + " subforms");
        }
        return size;
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


    final SyntaxSexp requiredSexp(String expectation, int argNum)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum);
        return checkSyntax(SyntaxSexp.class, expectation,
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


    // TODO problematic WRT keywords
    @Deprecated
    final SyntaxSymbol requiredSymbol(String expectation, int argNum)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum);
        return checkSyntax(SyntaxSymbol.class, expectation,
                           false /* nullable */, form);
    }


    final String requiredText(String expectation, int argNum)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum);
        return checkSyntax(SyntaxText.class, expectation,
                           false /* nullable */, form).stringValue();
    }


    final SyntaxSymbol requiredIdentifier(String expectation, int argNum)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum);
        // Special case for better error messaging
        if (form instanceof SyntaxKeyword)
        {
            String message =
                "expected " + expectation + ", not a keyword";
            throw failure(message, form);
        }
        // TODO check emptyness
        return checkSyntax(SyntaxSymbol.class, expectation,
                           false /* nullable */, form);
    }

    final SyntaxSymbol requiredIdentifier(int argNum)
        throws SyntaxFailure
    {
        return requiredIdentifier("identifier", argNum);
    }


    @Deprecated
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

        throw failure("expected " + expectation, form);
    }

    //========================================================================

    SyntaxChecker subformSexp(String description, int index)
        throws SyntaxFailure
    {
        SyntaxSexp subsexp = requiredSexp(description, index);
        return new SubformChecker(this, description, subsexp);
    }

    SyntaxChecker subformSeq(String description, int index)
        throws SyntaxFailure
    {
        SyntaxSequence subsexp = requiredSequence(description, index);
        return new SubformChecker(this, description, subsexp);
    }

    static class SubformChecker extends SyntaxChecker
    {
        private final SyntaxChecker myBaseForm;

        SubformChecker(SyntaxChecker base, String description,
                       SyntaxSequence form)
        {
            super(description, form);
            myBaseForm = base;
        }

        @Override
        SyntaxFailure failure(String message)
        {
            return new SyntaxFailure(myBaseForm.myFormName, message,
                                     myForm, myBaseForm.myForm);
        }

        @Override
        SyntaxFailure failure(String message, SyntaxValue subform)
        {
            return new SyntaxFailure(myBaseForm.myFormName, message,
                                     subform, myBaseForm.myForm);
        }

        @Override
        void arityExact(int count)
            throws SyntaxFailure
        {
            if (myForm.size() != count)
            {
                String message =
                    "expected " + count + " subforms in " + myFormName;
                throw failure(message);
            }
        }

        @Override
        SyntaxChecker subformSexp(String description, int index)
            throws SyntaxFailure
        {
            SyntaxSexp subsexp = requiredSexp(description, index);
            return new SubformChecker(myBaseForm, description, subsexp);
        }

        @Override
        SyntaxChecker subformSeq(String description, int index)
            throws SyntaxFailure
        {
            SyntaxSequence subsexp = requiredSequence(description, index);
            return new SubformChecker(myBaseForm, description, subsexp);
        }
    }
}
