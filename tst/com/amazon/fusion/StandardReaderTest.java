// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class StandardReaderTest
    extends CoreTestCase
{
    @Rule
    public ExpectedException thrown = ExpectedException.none();


    @Test
    public void testIonSyntaxError()
        throws Exception
    {
        useTstRepo();

        // TODO This should be more specific, but SyntaxException is oriented
        // around problems with Fusion syntax forms, not Ion data forms.
        thrown.expect(FusionErrorException.class);
        thrown.expectMessage("Error reading /malformed/ion_syntax_error");
        eval("(require '''/malformed/ion_syntax_error''')");
    }
}
