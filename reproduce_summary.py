"""Recalculate the reported feedback endpoints from supplied numeric records.

Run with Python 3 from this directory. Only the Python standard library is used.
The output is feedback_summary.csv in the same directory.
"""

import csv
import json
from math import isclose, isfinite
from pathlib import Path
from statistics import mean


def main():
    directory = Path(__file__).resolve().parent
    data = json.loads((directory / 'figure_source_data.json').read_text(encoding='utf-8'))
    with (directory / 'final_task_data.csv').open(newline='', encoding='utf-8-sig') as stream:
        records = list(csv.DictReader(stream))
    assert len(records) == 32, 'Expected two models, two groups and eight tasks per group'
    summary = []
    for group in (1, 2):
        group_results = {}
        for model, json_name in [('sobolev', 'sobolev'), ('ratio_weight', 'ratio_weight')]:
            rows = [row for row in records if int(row['group']) == group and row['model'] == model]
            rows.sort(key=lambda row: int(row['task_zero_based']))
            assert [int(row['task_zero_based']) for row in rows] == list(range(8))
            errors = [float(row['final_mae']) for row in rows]
            maxima = [float(row['final_max_axis_error']) for row in rows]
            assert all(isfinite(x) and x >= 0 for x in errors + maxima)
            archived = data[f'loop{group}']['comparison'][json_name]
            assert isclose(mean(errors), archived['mae_by_step'][-1], rel_tol=1e-10, abs_tol=1e-12)
            success_002 = sum(value <= 0.02 for value in maxima)
            success_0005 = sum(value <= 0.005 for value in maxima)
            assert success_002 == archived['success02_by_step'][-1]
            assert success_0005 == archived['success005_by_step'][-1]
            result = dict(group=group, model=model, tasks=8, step_budget=4,
                          endpoint_mae=mean(errors), success_at_002=success_002,
                          success_at_0005=success_0005)
            summary.append(result)
            group_results[model] = result
        factor = group_results['sobolev']['endpoint_mae'] / group_results['ratio_weight']['endpoint_mae']
        print(f'Group {group}: baseline-to-final mean-error ratio {factor:.2f}')
    final_rows = [row for row in summary if row['model'] == 'ratio_weight']
    assert sum(row['success_at_002'] for row in final_rows) == 15
    assert sum(row['success_at_0005'] for row in final_rows) == 12
    with (directory / 'feedback_summary.csv').open('w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(summary[0]))
        writer.writeheader()
        writer.writerows(summary)
    print('Verified 15/16 successes at 0.02 and 12/16 at 0.005')


if __name__ == '__main__':
    main()
