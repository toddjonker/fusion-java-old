// Copyright (c) 2021 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.util.hamt;

import static com.amazon.fusion.util.hamt.FunctionalHashTrie.empty;
import com.amazon.fusion.util.hamt.TransformTestCase.IncrementXform;
import org.junit.Ignore;
import org.junit.Test;

@SuppressWarnings({"rawtypes", "unchecked"})
public class EmptyHashTrieTest
    extends MultiHashTrieTestCase
{
    @Override
    MultiHashTrie simpleSubject()
    {
        return empty();
    }


    //=========================================================================
    // Inspection
    //   These are exercised extensively via hash() and other fixture methods.

    // containsKey()

    @Test @Ignore @Override
    public void containsKeyRejectsNullKey()
    {
        // FIXME
    }


    // get() -- Exercised via hash() and other fixture methods.

    @Test @Ignore @Override
    public void getRejectsNullKey()
    {
        // FIXME
    }


    //=========================================================================
    // Modification


    // withoutKey()

    @Test
    public void withoutKeyReturnsSingleton()
    {
        expectEmpty(empty().withoutKey("absent"));
    }


    // withoutKeys()

    @Test @Ignore
    @Override
    public void withoutKeysRejectsNullKey()
    {
        thrown.expect(NullPointerException.class);
        simpleSubject().withoutKeys(1, null, 3);
    }

    @Test
    public void withoutKeysReturnsSingleton()
    {
        expectEmpty(empty().withoutKeys("absent", "missing", -1));
    }


    // transform()

    @Test
    public void transformReturnsSingleton()
    {
        expectEmpty(empty().transform(new IncrementXform()));
    }
}
