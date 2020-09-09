// Copyright (c) 2019-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.unsafeStructSize;
import static org.junit.Assert.assertEquals;
import com.amazon.ion.IonReader;
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

    @Test
    public void testReadingStructWithRepeat()
        throws Exception
    {
        IonReader reader = system().newReader("{f:1,f:2}");
        reader.next();
        Object s = StandardReader.read(evaluator(), reader);
        assertEquals(2, unsafeStructSize(evaluator(), s));
    }
}
