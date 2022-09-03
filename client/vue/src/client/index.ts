

class Client {
    constructor() { }
    #getToken() {
        return localStorage.getItem('token');
    }
    #setToken(token: string) {
        localStorage.setItem('token', token);
    }
    #headers() {
        let token = this.#getToken();
        let headers = new Headers({});
        if (token) {
            console.log('HEADERS', token)
            console.log(`Bearer ${token}`)
            console.log(token);
            headers.append('Authorization', `Bearer ${token}`);
        }
        return headers;
    }
    #contentHeaders() {
        let headers = this.#headers();
        headers.append('Content-Type', 'application/json');
        return headers;
    }
    async #get(url: string) {
        return fetch(url, {
            method: 'GET',
            headers: this.#headers(),
        });
    }
    async #delete(url: string) {
        return fetch(url, {
            method: 'DELETE',
            headers: this.#headers(),
        });
    }
    async #post(url: string, body: any) {
        return fetch(url, {
            method: 'POST',
            body: JSON.stringify(body),
            headers: this.#contentHeaders(),
        });
    }
    async login(username: string, password: string) {
        let response = await this.#post('http://localhost:8000/login/', { username, password });
        let token = await response.json();
        this.#setToken(token);
    }
    async getBoards(): Promise<Array<any>> {
        let response = await this.#get('http://localhost:8000/games/');
        let json = await response.json();
        return json
    }
    async createNewBoard(size: number): Promise<number> {
        let response = await this.#post('http://localhost:8000/games/', { size });
        let json = await response.json();
        return json['id'];
    }
    async deleteBoard(id: number) {
        await this.#delete(`http://localhost:8000/games/${id}/`);
    }
    async getBoard(id: number) {
        let response = await this.#get(`http://localhost:8000/games/${id}/`);
        let json = await response.json();
        return json;
    }
    async playStone(id: number, x: number, y: number) {
        let response = await this.#post(`http://localhost:8000/games/${id}/play/`, { x, y });
        return await response.json();
    }
}

export default Client;