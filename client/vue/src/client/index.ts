class Client {
    constructor() { }
    async playStone(x: number, y: number) {
        return fetch('http://localhost:8000/games/1/play/', {
            method: 'POST',
            body: JSON.stringify({ x, y }),
            headers: new Headers({
                'Content-Type': 'application/json'
            })
        });
    }
}

export default Client;