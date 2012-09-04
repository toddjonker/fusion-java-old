// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Environment.Binding;
import java.util.Iterator;

/**
 * Linked-list of lexical information wrapped around a {@link SyntaxValue}
 * hierarchy.
 */
abstract class SyntaxWrap
{
    /**
     * @return not null
     */
    abstract Binding resolve(SyntaxSymbol ident,
                             Iterator<SyntaxWrap> moreWraps);
}
