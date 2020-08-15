import argparse

daymap = {
    'mon': '1',
    'tue': '2',
    'wed': '3',
    'thu': '4',
    'fri': '5',
    'sat': '6',
    'sun': '7'
}

MINUTES_IN_DAY = 1440


def convert(day, hour, minute):
    day -= 1
    return int(day * MINUTES_IN_DAY / 10 + hour * 60 / 10 + minute / 10)


if __name__ == '__main__':
    print('Running main function.')
    parser = argparse.ArgumentParser()
    parser.add_argument('day', type=str, choices=[
                        'mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun', '1', '2', '3', '4', '5', '6', '7'])
    parser.add_argument('hour', type=int)
    parser.add_argument('minute', type=int)
    args = parser.parse_args()
    print('Args: {}'.format(args))
    print(convert(int(
        daymap[args.day] if args.day in daymap else args.day), args.hour, args.minute))
