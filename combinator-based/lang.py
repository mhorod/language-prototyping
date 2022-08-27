from dataclasses import dataclass


@dataclass
class Source:
    filename: str
    text: str


@dataclass
class Location:
    source: Source
    begin: int
    end: int

    def __str__(self):
        return f"{self.source.filename}:{self.begin}..{self.end}"

    def length(self):
        return self.end - self.begin

    def join(locations):
        locations = list(locations)
        return Location(
            source=locations[0].source,
            begin=min(loc.begin for loc in locations),
            end=max(loc.end for loc in locations),
        )


@dataclass
class Token:
    kind: str
    text: str
    location: Location

    def __str__(self):
        return f"{self.kind}({repr(self.text)})@{self.location}"
