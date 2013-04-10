// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;

/**
 * Indicates an import of an identifier that is already bound.
 */
@SuppressWarnings("serial")
final class AmbiguousBindingFailure
    extends SyntaxFailure
{
    public AmbiguousBindingFailure(String whatForm, String identifier,
                                   SyntaxValue... exprs)
    {
        super(whatForm,
              "The identifier " + printQuotedSymbol(identifier) +
              " is already defined or imported from elsewhere",
              exprs);
    }
}
