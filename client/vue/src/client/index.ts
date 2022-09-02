class Client {
    constructor() { }
    async #get(url: string) {
        return fetch(url, {
            method: 'GET',
        });
    }
    async #post(url: string, body: any) {
        return fetch(url, {
            method: 'POST',
            body: JSON.stringify(body),
            headers: new Headers({
                'Content-Type': 'application/json'
            })
        });
    }
    async getBoard() {
        let response = await this.#get('http://localhost:8000/games/1/');
        let json = await response.json();
        return json['add'];
    }
    async playStone(x: number, y: number) {
        let response = await this.#post('http://localhost:8000/games/1/play/', { x, y });
        return await response.json();
    }
}

export default Client;