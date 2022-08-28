use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use symbolic_differentiator::interpreter::differentiate;

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("differentiate");
    let expressions = [
        "x^2".to_string(),
        "x^3 + 2 * x^2 - 4 * x + 3".to_string(),
        "sqrt(2 + x^2)".to_string(),
        "ln((1 + x)^3)".to_string(),
        "(x^2+z)^(y*z)+(a+b+c^x)-(8*x^2)".to_string(),
    ];
    for expression in expressions {
        
        // TODO: Count throughput by using the size of the tree of the expression
        group.throughput(Throughput::Elements(expression.len() as u64));
        
        group.bench_with_input(
            BenchmarkId::from_parameter(&expression),
            &expression,
            |bencher, expression| {
                bencher.iter(|| differentiate(expression.to_string(), "x".to_string()));
            },
        );
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
