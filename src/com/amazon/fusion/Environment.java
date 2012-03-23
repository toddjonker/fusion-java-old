// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Collection;


interface Environment
{    
    FusionValue lookup(String name);
    
    void collectNames(Collection<String> names);
}
