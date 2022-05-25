package com.lg4dc.metadata;

import java.util.UUID;
import com.bmartin.*;
import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;


@JsonAutoDetect(fieldVisibility = Visibility.ANY)
public class NoMetadata implements CborSerializable {
    public NoMetadata() {}
}
