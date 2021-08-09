from timeit import default_timer
from requests import Session
from concurrent.futures import ThreadPoolExecutor

import asyncio


START_TIME = default_timer()


csvs_to_fetch = [
    "ford_escort.csv",
    "cities.csv",
    "hw_25000.csv",
    "mlb_teams_2012.csv",
    "nile.csv",
    "homes.csv",
    "hooke.csv",
    "lead_shot.csv",
    "news_decline.csv",
    "snakes_count_10000.csv",
    "trees.csv",
    "zillow.csv"
]


def fetch(session: Session, csv: str):
    """ basic worker """
    base_url = "https://people.sc.fsu.edu/~jburkardt/data/csv/"

    with session.get(base_url + csv) as respose:
        data = respose.text

        if respose.status_code != 200:
            print("FAILURE::{0}".format(base_url + csv))

        elapsed = default_timer() - START_TIME
        time_completed_at = "{:5.2f}s".format(elapsed)
        print("{0:<30} {1:>20}".format(csv, time_completed_at))

        return data


async def get_data_asyn():
    print("{0:<30} {1:>20}".format("File", "Complete at"))

    with ThreadPoolExecutor(max_workers=10) as executor:
        with Session() as session:
            loop = asyncio.get_event_loop()

            global START_TIME
            START_TIME = default_timer()

            # list of working executor
            tasks = [
                loop.run_in_executor(
                    executor,
                    fetch,
                    *(session, csv)  # parameters.
                )
                for csv in csvs_to_fetch
            ]

            for respose in await asyncio.gather(*tasks):
                ...


if __name__ == "__main__":
    # create event loop
    loop = asyncio.get_event_loop()

    # promise
    future = asyncio.ensure_future(get_data_asyn())

    # run promise
    loop.run_until_complete(future)
