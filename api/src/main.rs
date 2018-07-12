extern crate actix_web;
use actix_web::{server, App, HttpRequest};
use std::cell::Cell;

struct AppState {
    counter: Cell<usize>,
}


fn index(req: HttpRequest<AppState>) -> String {
    // ex: getting / setting global state.
    let count = req.state().counter.get() + 1;
    req.state().counter.set(count);

    // what a request looks like.
    // println!("count is {:#?}", req); 

    // returned format with state vals.
    format!("Request count is: {}", count)
}


fn main() {
    println!("Todo Api Booted. Running on 8088...");

    server::new(
        || App::with_state(AppState {counter: Cell::new(0)})
        .resource("/api", |r| r.f(index)))
        .bind("127.0.0.1:8088")
        .unwrap()
        .run();

}
