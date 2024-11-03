import json
from abc import ABC
from dataclasses import dataclass
from enum import StrEnum
from pathlib import Path
from typing import Callable, Self, TypeVar
from uuid import UUID

import pyperf
from intervaltree import Interval, IntervalTree


class Density(StrEnum):
    sparse = "sparse"
    dense = "dense"


class DataCarrying(StrEnum):
    literal = "literal"
    data_carrying = "data-carrying"


class Intervalable(ABC):
    beginning: int
    end: int

    def to_interval(self) -> Interval:
        return Interval(begin=self.beginning, end=self.end, data=self)

    @classmethod
    def from_dict(cls, d: dict) -> Self:
        return cls(**d)


@dataclass(unsafe_hash=True)
class IntervalLit(Intervalable):
    beginning: int
    end: int


@dataclass
class DataCarryingInterval(Intervalable):
    beginning: int
    end: int
    id: UUID
    username: str
    key: str


I = TypeVar("I", bound=Intervalable)


def load_sample_data(density: Density, data_carrying: DataCarrying) -> list[Interval]:
    data_file_name = Path(f"{data_carrying}-{density}.jsonl")
    intervals_path = Path(__file__).parent / "test-data" / data_file_name
    with open(intervals_path, "r") as inf:
        return [
            loader_for(data_carrying)(json.loads(line)).to_interval()
            for line in inf.readlines()
        ]


def loader_for(data_carrying: DataCarrying) -> Callable[[dict], Intervalable]:
    match data_carrying:
        case DataCarrying.data_carrying:
            return DataCarryingInterval.from_dict
        case DataCarrying.literal:
            return IntervalLit.from_dict


def bench_construction(
    intervals: list[Interval], sizes: list[int], tags: list[str], runner: pyperf.Runner
) -> None:
    mode = " - ".join(tags)
    for size in sizes:
        sample = intervals[:size]
        tag = f"{mode} - {size}"
        runner.bench_func(tag, lambda: IntervalTree(sample))


def main():
    sizes = [100, 1000, 10000]
    runner = pyperf.Runner()
    for density in Density:
        for data_carrying in DataCarrying:
            bench_construction(
                intervals=load_sample_data(density, data_carrying),
                sizes=sizes,
                tags=[density, data_carrying],
                runner=runner,
            )


if __name__ == "__main__":
    main()
